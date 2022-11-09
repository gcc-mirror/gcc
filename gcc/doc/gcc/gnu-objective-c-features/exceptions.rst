..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exceptions:

Exceptions
**********

GNU Objective-C provides exception support built into the language, as
in the following example:

.. code-block:: objective-c

    @try {
      ...
         @throw expr;
      ...
    }
    @catch (AnObjCClass *exc) {
      ...
        @throw expr;
      ...
        @throw;
      ...
    }
    @catch (AnotherClass *exc) {
      ...
    }
    @catch (id allOthers) {
      ...
    }
    @finally {
      ...
        @throw expr;
      ...
    }

The ``@throw`` statement may appear anywhere in an Objective-C or
Objective-C++ program; when used inside of a ``@catch`` block, the
``@throw`` may appear without an argument (as shown above), in
which case the object caught by the ``@catch`` will be rethrown.

Note that only (pointers to) Objective-C objects may be thrown and
caught using this scheme.  When an object is thrown, it will be caught
by the nearest ``@catch`` clause capable of handling objects of
that type, analogously to how ``catch`` blocks work in C++ and
Java.  A ``@catch(id ...)`` clause (as shown above) may also
be provided to catch any and all Objective-C exceptions not caught by
previous ``@catch`` clauses (if any).

The ``@finally`` clause, if present, will be executed upon exit
from the immediately preceding ``@try ... @catch`` section.
This will happen regardless of whether any exceptions are thrown,
caught or rethrown inside the ``@try ... @catch`` section,
analogously to the behavior of the ``finally`` clause in Java.

There are several caveats to using the new exception mechanism:

* The :option:`-fobjc-exceptions` command line option must be used when
  compiling Objective-C files that use exceptions.

* With the GNU runtime, exceptions are always implemented as 'native'
  exceptions and it is recommended that the :option:`-fexceptions` and
  :option:`-shared-libgcc` options are used when linking.

* With the NeXT runtime, although currently designed to be binary
  compatible with ``NS_HANDLER`` -style idioms provided by the
  ``NSException`` class, the new exceptions can only be used on Mac
  OS X 10.3 (Panther) and later systems, due to additional functionality
  needed in the NeXT Objective-C runtime.

* As mentioned above, the new exceptions do not support handling
  types other than Objective-C objects.   Furthermore, when used from
  Objective-C++, the Objective-C exception model does not interoperate with C++
  exceptions at this time.  This means you cannot ``@throw`` an exception
  from Objective-C and ``catch`` it in C++, or vice versa
  (i.e., ``throw ... @catch``).
