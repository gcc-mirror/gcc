..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _messaging-with-the-gnu-objective-c-runtime:

Messaging with the GNU Objective-C Runtime
******************************************

This section is specific for the GNU Objective-C runtime.  If you are
using a different runtime, you can skip it.

The implementation of messaging in the GNU Objective-C runtime is
designed to be portable, and so is based on standard C.

Sending a message in the GNU Objective-C runtime is composed of two
separate steps.  First, there is a call to the lookup function,
``objc_msg_lookup ()`` (or, in the case of messages to super,
``objc_msg_lookup_super ()``).  This runtime function takes as
argument the receiver and the selector of the method to be called; it
returns the ``IMP``, that is a pointer to the function implementing
the method.  The second step of method invocation consists of casting
this pointer function to the appropriate function pointer type, and
calling the function pointed to it with the right arguments.

For example, when the compiler encounters a method invocation such as
``[object init]``, it compiles it into a call to
``objc_msg_lookup (object, @selector(init))`` followed by a cast
of the returned value to the appropriate function pointer type, and
then it calls it.

.. toctree::
  :maxdepth: 2


.. =========================================================================

.. _dynamically-registering-methods:

Dynamically Registering Methods
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If ``objc_msg_lookup()`` does not find a suitable method
implementation, because the receiver does not implement the required
method, it tries to see if the class can dynamically register the
method.

To do so, the runtime checks if the class of the receiver implements
the method

.. code-block:: objective-c

  + (BOOL) resolveInstanceMethod: (SEL)selector;

in the case of an instance method, or

.. code-block:: objective-c

  + (BOOL) resolveClassMethod: (SEL)selector;

in the case of a class method.  If the class implements it, the
runtime invokes it, passing as argument the selector of the original
method, and if it returns ``YES``, the runtime tries the lookup
again, which could now succeed if a matching method was added
dynamically by ``+resolveInstanceMethod:`` or
``+resolveClassMethod:``.

This allows classes to dynamically register methods (by adding them to
the class using ``class_addMethod``) when they are first called.
To do so, a class should implement ``+resolveInstanceMethod:`` (or,
depending on the case, ``+resolveClassMethod:``) and have it
recognize the selectors of methods that can be registered dynamically
at runtime, register them, and return ``YES``.  It should return
``NO`` for methods that it does not dynamically registered at
runtime.

If ``+resolveInstanceMethod:`` (or ``+resolveClassMethod:``) is
not implemented or returns ``NO``, the runtime then tries the
forwarding hook.

Support for ``+resolveInstanceMethod:`` and
``resolveClassMethod:`` was added to the GNU Objective-C runtime in
GCC version 4.6.

.. =========================================================================

.. _forwarding-hook:

Forwarding Hook
^^^^^^^^^^^^^^^

The GNU Objective-C runtime provides a hook, called
``__objc_msg_forward2``, which is called by
``objc_msg_lookup()`` when it cannot find a method implementation in
the runtime tables and after calling ``+resolveInstanceMethod:``
and ``+resolveClassMethod:`` has been attempted and did not succeed
in dynamically registering the method.

To configure the hook, you set the global variable
``__objc_msg_forward2`` to a function with the same argument and
return types of ``objc_msg_lookup()``.  When
``objc_msg_lookup()`` cannot find a method implementation, it
invokes the hook function you provided to get a method implementation
to return.  So, in practice ``__objc_msg_forward2`` allows you to
extend ``objc_msg_lookup()`` by adding some custom code that is
called to do a further lookup when no standard method implementation
can be found using the normal lookup.

This hook is generally reserved for 'Foundation' libraries such as
GNUstep Base, which use it to implement their high-level method
forwarding API, typically based around the ``forwardInvocation:``
method.  So, unless you are implementing your own 'Foundation'
library, you should not set this hook.

In a typical forwarding implementation, the ``__objc_msg_forward2``
hook function determines the argument and return type of the method
that is being looked up, and then creates a function that takes these
arguments and has that return type, and returns it to the caller.
Creating this function is non-trivial and is typically performed using
a dedicated library such as ``libffi``.

The forwarding method implementation thus created is returned by
``objc_msg_lookup()`` and is executed as if it was a normal method
implementation.  When the forwarding method implementation is called,
it is usually expected to pack all arguments into some sort of object
(typically, an ``NSInvocation`` in a 'Foundation' library), and
hand it over to the programmer (``forwardInvocation:``) who is then
allowed to manipulate the method invocation using a high-level API
provided by the 'Foundation' library.  For example, the programmer
may want to examine the method invocation arguments and name and
potentially change them before forwarding the method invocation to one
or more local objects (``performInvocation:``) or even to remote
objects (by using Distributed Objects or some other mechanism).  When
all this completes, the return value is passed back and must be
returned correctly to the original caller.

Note that the GNU Objective-C runtime currently provides no support
for method forwarding or method invocations other than the
``__objc_msg_forward2`` hook.

If the forwarding hook does not exist or returns ``NULL``, the
runtime currently attempts forwarding using an older, deprecated API,
and if that fails, it aborts the program.  In future versions of the
GNU Objective-C runtime, the runtime will immediately abort.