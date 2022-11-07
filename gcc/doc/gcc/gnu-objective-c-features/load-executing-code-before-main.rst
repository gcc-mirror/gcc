..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _executing-code-before-main:

+load: Executing Code before main
*********************************

This section is specific for the GNU Objective-C runtime.  If you are
using a different runtime, you can skip it.

The GNU Objective-C runtime provides a way that allows you to execute
code before the execution of the program enters the ``main``
function.  The code is executed on a per-class and a per-category basis,
through a special class method ``+load``.

This facility is very useful if you want to initialize global variables
which can be accessed by the program directly, without sending a message
to the class first.  The usual way to initialize global variables, in the
``+initialize`` method, might not be useful because
``+initialize`` is only called when the first message is sent to a
class object, which in some cases could be too late.

Suppose for example you have a ``FileStream`` class that declares
``Stdin``, ``Stdout`` and ``Stderr`` as global variables, like
below:

.. code-block:: objective-c

  FileStream *Stdin = nil;
  FileStream *Stdout = nil;
  FileStream *Stderr = nil;

  @implementation FileStream

  + (void)initialize
  {
      Stdin = [[FileStream new] initWithFd:0];
      Stdout = [[FileStream new] initWithFd:1];
      Stderr = [[FileStream new] initWithFd:2];
  }

  /* Other methods here */
  @end

In this example, the initialization of ``Stdin``, ``Stdout`` and
``Stderr`` in ``+initialize`` occurs too late.  The programmer can
send a message to one of these objects before the variables are actually
initialized, thus sending messages to the ``nil`` object.  The
``+initialize`` method which actually initializes the global
variables is not invoked until the first message is sent to the class
object.  The solution would require these variables to be initialized
just before entering ``main``.

The correct solution of the above problem is to use the ``+load``
method instead of ``+initialize`` :

.. code-block:: objective-c

  @implementation FileStream

  + (void)load
  {
      Stdin = [[FileStream new] initWithFd:0];
      Stdout = [[FileStream new] initWithFd:1];
      Stderr = [[FileStream new] initWithFd:2];
  }

  /* Other methods here */
  @end

The ``+load`` is a method that is not overridden by categories.  If a
class and a category of it both implement ``+load``, both methods are
invoked.  This allows some additional initializations to be performed in
a category.

This mechanism is not intended to be a replacement for ``+initialize``.
You should be aware of its limitations when you decide to use it
instead of ``+initialize``.

.. toctree::
  :maxdepth: 2


.. _what-you-can-and-what-you-cannot-do-in-+load:

What You Can and Cannot Do in +load
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``+load`` is to be used only as a last resort.  Because it is
executed very early, most of the Objective-C runtime machinery will
not be ready when ``+load`` is executed; hence ``+load`` works
best for executing C code that is independent on the Objective-C
runtime.

The ``+load`` implementation in the GNU runtime guarantees you the
following things:

* you can write whatever C code you like;

* you can allocate and send messages to objects whose class is implemented
  in the same file;

* the ``+load`` implementation of all super classes of a class are
  executed before the ``+load`` of that class is executed;

* the ``+load`` implementation of a class is executed before the
  ``+load`` implementation of any category.

In particular, the following things, even if they can work in a
particular case, are not guaranteed:

* allocation of or sending messages to arbitrary objects;

* allocation of or sending messages to objects whose classes have a
  category implemented in the same file;

* sending messages to Objective-C constant strings (``@"this is a
  constant string"``);

You should make no assumptions about receiving ``+load`` in sibling
classes when you write ``+load`` of a class.  The order in which
sibling classes receive ``+load`` is not guaranteed.

The order in which ``+load`` and ``+initialize`` are called could
be problematic if this matters.  If you don't allocate objects inside
``+load``, it is guaranteed that ``+load`` is called before
``+initialize``.  If you create an object inside ``+load`` the
``+initialize`` method of object's class is invoked even if
``+load`` was not invoked.  Note if you explicitly call ``+load``
on a class, ``+initialize`` will be called first.  To avoid possible
problems try to implement only one of these methods.

The ``+load`` method is also invoked when a bundle is dynamically
loaded into your running program.  This happens automatically without any
intervening operation from you.  When you write bundles and you need to
write ``+load`` you can safely create and send messages to objects whose
classes already exist in the running program.  The same restrictions as
above apply to classes defined in bundle.