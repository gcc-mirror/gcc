..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _synchronization:

Synchronization
***************

GNU Objective-C provides support for synchronized blocks:

.. code-block:: objective-c

    @synchronized (ObjCClass *guard) {
      ...
    }

Upon entering the ``@synchronized`` block, a thread of execution
shall first check whether a lock has been placed on the corresponding
``guard`` object by another thread.  If it has, the current thread
shall wait until the other thread relinquishes its lock.  Once
``guard`` becomes available, the current thread will place its own
lock on it, execute the code contained in the ``@synchronized``
block, and finally relinquish the lock (thereby making ``guard``
available to other threads).

Unlike Java, Objective-C does not allow for entire methods to be
marked ``@synchronized``.  Note that throwing exceptions out of
``@synchronized`` blocks is allowed, and will cause the guarding
object to be unlocked properly.

Because of the interactions between synchronization and exception
handling, you can only use ``@synchronized`` when compiling with
exceptions enabled, that is with the command line option
:option:`-fobjc-exceptions`.