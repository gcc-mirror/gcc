..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gnu-objective-c-runtime-api:

GNU Objective-C Runtime API
***************************

This section is specific for the GNU Objective-C runtime.  If you are
using a different runtime, you can skip it.

The GNU Objective-C runtime provides an API that allows you to
interact with the Objective-C runtime system, querying the live
runtime structures and even manipulating them.  This allows you for
example to inspect and navigate classes, methods and protocols; to
define new classes or new methods, and even to modify existing classes
or protocols.

If you are using a 'Foundation' library such as GNUstep-Base, this
library will provide you with a rich set of functionality to do most
of the inspection tasks, and you probably will only need direct access
to the GNU Objective-C runtime API to define new classes or methods.

.. toctree::
  :maxdepth: 2


.. =========================================================================

.. _modern-gnu-objective-c-runtime-api:

Modern GNU Objective-C Runtime API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GNU Objective-C runtime provides an API which is similar to the
one provided by the 'Objective-C 2.0' Apple/NeXT Objective-C
runtime.  The API is documented in the public header files of the GNU
Objective-C runtime:

* :samp:`objc/objc.h`: this is the basic Objective-C header file,
  defining the basic Objective-C types such as ``id``, ``Class``
  and ``BOOL``.  You have to include this header to do almost
  anything with Objective-C.

* :samp:`objc/runtime.h`: this header declares most of the public runtime
  API functions allowing you to inspect and manipulate the Objective-C
  runtime data structures.  These functions are fairly standardized
  across Objective-C runtimes and are almost identical to the Apple/NeXT
  Objective-C runtime ones.  It does not declare functions in some
  specialized areas (constructing and forwarding message invocations,
  threading) which are in the other headers below.  You have to include
  :samp:`objc/objc.h` and :samp:`objc/runtime.h` to use any of the
  functions, such as ``class_getName()``, declared in
  :samp:`objc/runtime.h`.

* :samp:`objc/message.h`: this header declares public functions used to
  construct, deconstruct and forward message invocations.  Because
  messaging is done in quite a different way on different runtimes,
  functions in this header are specific to the GNU Objective-C runtime
  implementation.

* :samp:`objc/objc-exception.h`: this header declares some public
  functions related to Objective-C exceptions.  For example functions in
  this header allow you to throw an Objective-C exception from plain
  C/C++ code.

* :samp:`objc/objc-sync.h`: this header declares some public functions
  related to the Objective-C ``@synchronized()`` syntax, allowing
  you to emulate an Objective-C ``@synchronized()`` block in plain
  C/C++ code.

* :samp:`objc/thr.h`: this header declares a public runtime API threading
  layer that is only provided by the GNU Objective-C runtime.  It
  declares functions such as ``objc_mutex_lock()``, which provide a
  platform-independent set of threading functions.

The header files contain detailed documentation for each function in
the GNU Objective-C runtime API.

.. =========================================================================

.. _traditional-gnu-objective-c-runtime-api:

Traditional GNU Objective-C Runtime API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GNU Objective-C runtime used to provide a different API, which we
call the 'traditional' GNU Objective-C runtime API.  Functions
belonging to this API are easy to recognize because they use a
different naming convention, such as ``class_get_super_class()``
(traditional API) instead of ``class_getSuperclass()`` (modern
API).  Software using this API includes the file
:samp:`objc/objc-api.h` where it is declared.

Starting with GCC 4.7.0, the traditional GNU runtime API is no longer
available.
