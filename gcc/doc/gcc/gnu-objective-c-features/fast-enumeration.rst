..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _fast-enumeration:

Fast Enumeration
****************

.. toctree::
  :maxdepth: 2


.. ================================

.. _using-fast-enumeration:

Using Fast Enumeration
^^^^^^^^^^^^^^^^^^^^^^

GNU Objective-C provides support for the fast enumeration syntax:

.. code-block:: objective-c

    id array = ...;
    id object;

    for (object in array)
    {
      /* Do something with 'object' */
    }

``array`` needs to be an Objective-C object (usually a collection
object, for example an array, a dictionary or a set) which implements
the 'Fast Enumeration Protocol' (see below).  If you are using a
Foundation library such as GNUstep Base or Apple Cocoa Foundation, all
collection objects in the library implement this protocol and can be
used in this way.

The code above would iterate over all objects in ``array``.  For
each of them, it assigns it to ``object``, then executes the
``Do something with 'object'`` statements.

Here is a fully worked-out example using a Foundation library (which
provides the implementation of ``NSArray``, ``NSString`` and
``NSLog``):

.. code-block:: objective-c

    NSArray *array = [NSArray arrayWithObjects: @"1", @"2", @"3", nil];
    NSString *object;

    for (object in array)
      NSLog (@"Iterating over %@", object);

.. ================================

.. _c99-like-fast-enumeration-syntax:

C99-Like Fast Enumeration Syntax
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A c99-like declaration syntax is also allowed:

.. code-block:: objective-c

    id array = ...;

    for (id object in array)
    {
      /* Do something with 'object'  */
    }

this is completely equivalent to:

.. code-block:: objective-c

    id array = ...;

    {
      id object;
      for (object in array)
      {
        /* Do something with 'object'  */
      }
    }

but can save some typing.

Note that the option :option:`-std=c99` is not required to allow this
syntax in Objective-C.

.. ================================

.. _fast-enumeration-details:

Fast Enumeration Details
^^^^^^^^^^^^^^^^^^^^^^^^

Here is a more technical description with the gory details.  Consider the code

.. code-block:: objective-c

    for (object expression in collection expression)
    {
      statements
    }

here is what happens when you run it:

* ``collection expression`` is evaluated exactly once and the
  result is used as the collection object to iterate over.  This means
  it is safe to write code such as ``for (object in [NSDictionary
  keyEnumerator]) ...``.

* the iteration is implemented by the compiler by repeatedly getting
  batches of objects from the collection object using the fast
  enumeration protocol (see below), then iterating over all objects in
  the batch.  This is faster than a normal enumeration where objects are
  retrieved one by one (hence the name 'fast enumeration').

* if there are no objects in the collection, then
  ``object expression`` is set to ``nil`` and the loop
  immediately terminates.

* if there are objects in the collection, then for each object in the
  collection (in the order they are returned) ``object expression``
  is set to the object, then ``statements`` are executed.

* ``statements`` can contain ``break`` and ``continue``
  commands, which will abort the iteration or skip to the next loop
  iteration as expected.

* when the iteration ends because there are no more objects to iterate
  over, ``object expression`` is set to ``nil``.  This allows
  you to determine whether the iteration finished because a ``break``
  command was used (in which case ``object expression`` will remain
  set to the last object that was iterated over) or because it iterated
  over all the objects (in which case ``object expression`` will be
  set to ``nil``).

* ``statements`` must not make any changes to the collection
  object; if they do, it is a hard error and the fast enumeration
  terminates by invoking ``objc_enumerationMutation``, a runtime
  function that normally aborts the program but which can be customized
  by Foundation libraries via ``objc_set_mutation_handler`` to do
  something different, such as raising an exception.

.. ================================

.. _fast-enumeration-protocol:

Fast Enumeration Protocol
^^^^^^^^^^^^^^^^^^^^^^^^^

If you want your own collection object to be usable with fast
enumeration, you need to have it implement the method

.. code-block::

  - (unsigned long) countByEnumeratingWithState: (NSFastEnumerationState \*)state
                                        objects: (id \*)objects
                                          count: (unsigned long)len;

where ``NSFastEnumerationState`` must be defined in your code as follows:

.. code-block:: objective-c

  typedef struct
  {
    unsigned long state;
    id            *itemsPtr;
    unsigned long *mutationsPtr;
    unsigned long extra[5];
  } NSFastEnumerationState;

If no ``NSFastEnumerationState`` is defined in your code, the
compiler will automatically replace ``NSFastEnumerationState *``
with ``struct __objcFastEnumerationState *``, where that type is
silently defined by the compiler in an identical way.  This can be
confusing and we recommend that you define
``NSFastEnumerationState`` (as shown above) instead.

The method is called repeatedly during a fast enumeration to retrieve
batches of objects.  Each invocation of the method should retrieve the
next batch of objects.

The return value of the method is the number of objects in the current
batch; this should not exceed ``len``, which is the maximum size of
a batch as requested by the caller.  The batch itself is returned in
the ``itemsPtr`` field of the ``NSFastEnumerationState`` struct.

To help with returning the objects, the ``objects`` array is a C
array preallocated by the caller (on the stack) of size ``len``.
In many cases you can put the objects you want to return in that
``objects`` array, then do ``itemsPtr = objects``.  But you
don't have to; if your collection already has the objects to return in
some form of C array, it could return them from there instead.

The ``state`` and ``extra`` fields of the
``NSFastEnumerationState`` structure allows your collection object
to keep track of the state of the enumeration.  In a simple array
implementation, ``state`` may keep track of the index of the last
object that was returned, and ``extra`` may be unused.

The ``mutationsPtr`` field of the ``NSFastEnumerationState`` is
used to keep track of mutations.  It should point to a number; before
working on each object, the fast enumeration loop will check that this
number has not changed.  If it has, a mutation has happened and the
fast enumeration will abort.  So, ``mutationsPtr`` could be set to
point to some sort of version number of your collection, which is
increased by one every time there is a change (for example when an
object is added or removed).  Or, if you are content with less strict
mutation checks, it could point to the number of objects in your
collection or some other value that can be checked to perform an
approximate check that the collection has not been mutated.

Finally, note how we declared the ``len`` argument and the return
value to be of type ``unsigned long``.  They could also be declared
to be of type ``unsigned int`` and everything would still work.