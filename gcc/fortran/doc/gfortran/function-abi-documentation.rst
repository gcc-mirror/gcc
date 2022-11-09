..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _function-abi-documentation:

Function ABI Documentation
**************************

.. toctree::
  :maxdepth: 2


.. index:: Coarray, _gfortran_caf_init

.. _gfortran_caf_init:

_gfortran_caf_init --- Initialiation function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_init (int *argc, char ***argv)

  This function is called at startup of the program before the Fortran main
  program, if the latter has been compiled with :option:`-fcoarray=lib`.
  It takes as arguments the command-line arguments of the program.  It is
  permitted to pass two ``NULL`` pointers as argument; if non- ``NULL``,
  the library is permitted to modify the arguments.

  :param argc:
    intent(inout) An integer pointer with the number of
    arguments passed to the program or ``NULL``.

  :param argv:
    intent(inout) A pointer to an array of strings with the
    command-line arguments or ``NULL``.

  .. note::

    The function is modelled after the initialization function of the Message
    Passing Interface (MPI) specification.  Due to the way coarray registration
    works, it might not be the first call to the library.  If the main program is
    not written in Fortran and only a library uses coarrays, it can happen that
    this function is never called.  Therefore, it is recommended that the library
    does not rely on the passed arguments and whether the call has been done.

.. index:: Coarray, _gfortran_caf_finish

.. _gfortran_caf_finish:

_gfortran_caf_finish --- Finalization function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_finish (void)

  This function is called at the end of the Fortran main program, if it has
  been compiled with the :option:`-fcoarray=lib` option.

  .. note::

    For non-Fortran programs, it is recommended to call the function at the end
    of the main program.  To ensure that the shutdown is also performed for
    programs where this function is not explicitly invoked, for instance
    non-Fortran programs or calls to the system's exit() function, the library
    can use a destructor function.  Note that programs can also be terminated
    using the STOP and ERROR STOP statements; those use different library calls.

.. index:: Coarray, _gfortran_caf_this_image

.. _gfortran_caf_this_image:

_gfortran_caf_this_image --- Querying the image number
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int _gfortran_caf_this_image (int distance)

  This function returns the current image number, which is a positive number.

  :param distance:
    As specified for the ``this_image`` intrinsic
    in TS18508.  Shall be a non-negative number.

  .. note::

    If the Fortran intrinsic ``this_image`` is invoked without an argument, which
    is the only permitted form in Fortran 2008, GCC passes ``0`` as
    first argument.

.. index:: Coarray, _gfortran_caf_num_images

.. _gfortran_caf_num_images:

_gfortran_caf_num_images --- Querying the maximal number of images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int _gfortran_caf_num_images(int distance, int failed)

  This function returns the number of images in the current team, if
  :samp:`{distance}` is 0 or the number of images in the parent team at the specified
  distance. If failed is -1, the function returns the number of all images at
  the specified distance; if it is 0, the function returns the number of
  nonfailed images, and if it is 1, it returns the number of failed images.

  :param distance:
    the distance from this image to the ancestor.
    Shall be positive.

  :param failed:
    shall be -1, 0, or 1

  .. note::

    This function follows TS18508. If the num_image intrinsic has no arguments,
    then the compiler passes ``distance=0`` and ``failed=-1`` to the function.

.. index:: Coarray, _gfortran_caf_image_status

.. _gfortran_caf_image_status:

_gfortran_caf_image_status --- Query the status of an image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int _gfortran_caf_image_status (int image, caf_team_t * team)

  Get the status of the image given by the id :samp:`{image}` of the team given by
  :samp:`{team}`.  Valid results are zero, for image is ok, ``STAT_STOPPED_IMAGE``
  from the ISO_FORTRAN_ENV module to indicate that the image has been stopped and
  ``STAT_FAILED_IMAGE`` also from ISO_FORTRAN_ENV to indicate that the image
  has executed a ``FAIL IMAGE`` statement.

  :param image:
    the positive scalar id of the image in the current TEAM.

  :param team:
    optional; team on the which the inquiry is to be
    performed.

  .. note::

    This function follows TS18508.  Because team-functionality is not yet
    implemented a null-pointer is passed for the :samp:`{team}` argument at the moment.

.. index:: Coarray, _gfortran_caf_failed_images

.. _gfortran_caf_failed_images:

_gfortran_caf_failed_images --- Get an array of the indexes of the failed images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int _gfortran_caf_failed_images (caf_team_t * team, int * kind)

  Get an array of image indexes in the current :samp:`{team}` that have failed.  The
  array is sorted ascendingly.  When :samp:`{team}` is not provided the current team
  is to be used.  When :samp:`{kind}` is provided then the resulting array is of that
  integer kind else it is of default integer kind.  The returns an unallocated
  size zero array when no images have failed.

  :param team:
    optional; team on the which the inquiry is to be
    performed.

  :param image:
    optional; the kind of the resulting integer array.

  .. note::

    This function follows TS18508.  Because team-functionality is not yet
    implemented a null-pointer is passed for the :samp:`{team}` argument at the moment.

.. index:: Coarray, _gfortran_caf_stopped_images

.. _gfortran_caf_stopped_images:

_gfortran_caf_stopped_images --- Get an array of the indexes of the stopped images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int _gfortran_caf_stopped_images (caf_team_t * team, int * kind)

  Get an array of image indexes in the current :samp:`{team}` that have stopped.  The
  array is sorted ascendingly.  When :samp:`{team}` is not provided the current team
  is to be used.  When :samp:`{kind}` is provided then the resulting array is of that
  integer kind else it is of default integer kind.  The returns an unallocated
  size zero array when no images have failed.

  :param team:
    optional; team on the which the inquiry is to be
    performed.

  :param image:
    optional; the kind of the resulting integer array.

  .. note::

    This function follows TS18508.  Because team-functionality is not yet
    implemented a null-pointer is passed for the :samp:`{team}` argument at the moment.

.. index:: Coarray, _gfortran_caf_register

.. _gfortran_caf_register:

_gfortran_caf_register --- Registering coarrays
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void caf_register (size_t size, caf_register_t type, caf_token_t *token, gfc_descriptor_t *desc, int *stat, char *errmsg, size_t errmsg_len)

  Registers memory for a coarray and creates a token to identify the coarray.  The
  routine is called for both coarrays with ``SAVE`` attribute and using an
  explicit ``ALLOCATE`` statement.  If an error occurs and :samp:`{STAT}` is a
  ``NULL`` pointer, the function shall abort with printing an error message
  and starting the error termination.  If no error occurs and :samp:`{STAT}` is
  present, it shall be set to zero.  Otherwise, it shall be set to a positive
  value and, if not- ``NULL``, :samp:`{ERRMSG}` shall be set to a string describing
  the failure.  The routine shall register the memory provided in the
  ``DATA`` -component of the array descriptor :samp:`{DESC}`, when that component
  is non- ``NULL``, else it shall allocate sufficient memory and provide a
  pointer to it in the ``DATA`` -component of :samp:`{DESC}`.  The array descriptor
  has rank zero, when a scalar object is to be registered and the array
  descriptor may be invalid after the call to ``_gfortran_caf_register``.
  When an array is to be allocated the descriptor persists.

  :param size:
    For normal coarrays, the byte size of the coarray to be
    allocated; for lock types and event types, the number of elements.

  :param type:
    one of the caf_register_t types.

  :param token:
    intent(out) An opaque pointer identifying the coarray.

  :param desc:
    intent(inout) The (pseudo) array descriptor.

  :param stat:
    intent(out) For allocatable coarrays, stores the STAT=;
    may be ``NULL``

  :param errmsg:
    intent(out) When an error occurs, this will be set to
    an error message; may be ``NULL``

  :param errmsg_len:
    the buffer size of errmsg.

  .. note::

    Nonallocatable coarrays have to be registered prior use from remote images.
    In order to guarantee this, they have to be registered before the main
    program. This can be achieved by creating constructor functions. That is what
    GCC does such that also for nonallocatable coarrays the memory is allocated and
    no static memory is used.  The token permits to identify the coarray; to the
    processor, the token is a nonaliasing pointer. The library can, for instance,
    store the base address of the coarray in the token, some handle or a more
    complicated struct.  The library may also store the array descriptor
    :samp:`{DESC}` when its rank is non-zero.

    For lock types, the value shall only be used for checking the allocation
    status. Note that for critical blocks, the locking is only required on one
    image; in the locking statement, the processor shall always pass an
    image index of one for critical-block lock variables
    (``CAF_REGTYPE_CRITICAL``). For lock types and critical-block variables,
    the initial value shall be unlocked (or, respectively, not in critical
    section) such as the value false; for event types, the initial state should
    be no event, e.g. zero.

.. index:: Coarray, _gfortran_caf_deregister

.. _gfortran_caf_deregister:

_gfortran_caf_deregister --- Deregistering coarrays
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void caf_deregister (caf_token_t *token, caf_deregister_t type, int *stat, char *errmsg, size_t errmsg_len)

  Called to free or deregister the memory of a coarray; the processor calls this
  function for automatic and explicit deallocation.  In case of an error, this
  function shall fail with an error message, unless the :samp:`{STAT}` variable is
  not null.  The library is only expected to free memory it allocated itself
  during a call to ``_gfortran_caf_register``.

  :param token:
    the token to free.

  :param type:
    the type of action to take for the coarray.  A
    ``CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY`` is allowed only for allocatable or
    pointer components of derived type coarrays.  The action only deallocates the
    local memory without deleting the token.

  :param stat:
    intent(out) Stores the STAT=; may be NULL

  :param errmsg:
    intent(out) When an error occurs, this will be set
    to an error message; may be NULL

  :param errmsg_len:
    the buffer size of errmsg.

  .. note::

    For nonalloatable coarrays this function is never called.  If a cleanup is
    required, it has to be handled via the finish, stop and error stop functions,
    and via destructors.

.. index:: Coarray, _gfortran_caf_is_present

.. _gfortran_caf_is_present:

_gfortran_caf_is_present --- Query whether an allocatable or pointer component in a derived type coarray is allocated
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_is_present (caf_token_t token, int image_index, gfc_reference_t *ref)

  Used to query the coarray library whether an allocatable component in a derived
  type coarray is allocated on a remote image.

  :param token:
    An opaque pointer identifying the coarray.

  :param image_index:
    The ID of the remote image; must be a positive
    number.

  :param ref:
    A chain of references to address the allocatable or
    pointer component in the derived type coarray.  The object reference needs to be
    a scalar or a full array reference, respectively.

.. index:: Coarray, _gfortran_caf_send

.. _gfortran_caf_send:

_gfortran_caf_send --- Sending data from a local image to a remote image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_send (caf_token_t token, size_t offset, int image_index, gfc_descriptor_t *dest, caf_vector_t *dst_vector, gfc_descriptor_t *src, int dst_kind, int src_kind, bool may_require_tmp, int *stat)

  Called to send a scalar, an array section or a whole array from a local
  to a remote image identified by the image_index.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param dest:
    intent(in)  Array descriptor for the remote image for the
    bounds and the size.  The ``base_addr`` shall not be accessed.

  :param dst_vector:
    intent(in)  If not NULL, it contains the vector
    subscript of the destination array; the values are relative to the dimension
    triplet of the dest argument.

  :param src:
    intent(in)  Array descriptor of the local array to be
    transferred to the remote image

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param stat:
    intent(out) when non-NULL give the result of the
    operation, i.e., zero on success and non-zero on error.  When NULL and an error
    occurs, then an error message is printed and the program is terminated.

  .. note::

    It is permitted to have :samp:`{image_index}` equal the current image; the memory
    of the send-to and the send-from might (partially) overlap in that case.  The
    implementation has to take care that it handles this case, e.g. using
    ``memmove`` which handles (partially) overlapping memory. If
    :samp:`{may_require_tmp}` is true, the library might additionally create a
    temporary variable, unless additional checks show that this is not required
    (e.g. because walking backward is possible or because both arrays are
    contiguous and ``memmove`` takes care of overlap issues).

    Note that the assignment of a scalar to an array is permitted. In addition,
    the library has to handle numeric-type conversion and for strings, padding
    and different character kinds.

.. index:: Coarray, _gfortran_caf_get

.. _gfortran_caf_get:

_gfortran_caf_get --- Getting data from a remote image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_get (caf_token_t token, size_t offset, int image_index, gfc_descriptor_t *src, caf_vector_t *src_vector, gfc_descriptor_t *dest, int src_kind, int dst_kind, bool may_require_tmp, int *stat)

  Called to get an array section or a whole array from a remote,
  image identified by the image_index.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param dest:
    intent(out) Array descriptor of the local array to store
    the data retrieved from the remote image

  :param src:
    intent(in) Array descriptor for the remote image for the
    bounds and the size.  The ``base_addr`` shall not be accessed.

  :param src_vector:
    intent(in)  If not NULL, it contains the vector
    subscript of the source array; the values are relative to the dimension
    triplet of the :samp:`{src}` argument.

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param stat:
    intent(out) When non-NULL give the result of the
    operation, i.e., zero on success and non-zero on error.  When NULL and an error
    occurs, then an error message is printed and the program is terminated.

  .. note::

    It is permitted to have :samp:`{image_index}` equal the current image; the memory of
    the send-to and the send-from might (partially) overlap in that case.  The
    implementation has to take care that it handles this case, e.g. using
    ``memmove`` which handles (partially) overlapping memory. If
    :samp:`{may_require_tmp}` is true, the library might additionally create a
    temporary variable, unless additional checks show that this is not required
    (e.g. because walking backward is possible or because both arrays are
    contiguous and ``memmove`` takes care of overlap issues).

    Note that the library has to handle numeric-type conversion and for strings,
    padding and different character kinds.

.. index:: Coarray, _gfortran_caf_sendget

.. _gfortran_caf_sendget:

_gfortran_caf_sendget --- Sending data between remote images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_sendget (caf_token_t dst_token, size_t dst_offset, int dst_image_index, gfc_descriptor_t *dest, caf_vector_t *dst_vector, caf_token_t src_token, size_t src_offset, int src_image_index, gfc_descriptor_t *src, caf_vector_t *src_vector, int dst_kind, int src_kind, bool may_require_tmp, int *stat)

  Called to send a scalar, an array section or a whole array from a remote image
  identified by the :samp:`{src_image_index}` to a remote image identified by the
  :samp:`{dst_image_index}`.

  :param dst_token:
    intent(in)  An opaque pointer identifying the
    destination coarray.

  :param dst_offset:
    intent(in)  By which amount of bytes the actual data
    is shifted compared to the base address of the destination coarray.

  :param dst_image_index:
    intent(in)  The ID of the destination remote
    image; must be a positive number.

  :param dest:
    intent(in) Array descriptor for the destination
    remote image for the bounds and the size.  The ``base_addr`` shall not be
    accessed.

  :param dst_vector:
    intent(int)  If not NULL, it contains the vector
    subscript of the destination array; the values are relative to the dimension
    triplet of the :samp:`{dest}` argument.

  :param src_token:
    intent(in)  An opaque pointer identifying the source
    coarray.

  :param src_offset:
    intent(in)  By which amount of bytes the actual data
    is shifted compared to the base address of the source coarray.

  :param src_image_index:
    intent(in)  The ID of the source remote image;
    must be a positive number.

  :param src:
    intent(in) Array descriptor of the local array to be
    transferred to the remote image.

  :param src_vector:
    intent(in) Array descriptor of the local array to
    be transferred to the remote image

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param stat:
    intent(out) when non-NULL give the result of the
    operation, i.e., zero on success and non-zero on error.  When NULL and an error
    occurs, then an error message is printed and the program is terminated.

  .. note::

    It is permitted to have the same image index for both :samp:`{src_image_index}` and
    :samp:`{dst_image_index}` ; the memory of the send-to and the send-from might
    (partially) overlap in that case.  The implementation has to take care that it
    handles this case, e.g. using ``memmove`` which handles (partially)
    overlapping memory.  If :samp:`{may_require_tmp}` is true, the library
    might additionally create a temporary variable, unless additional checks show
    that this is not required (e.g. because walking backward is possible or because
    both arrays are contiguous and ``memmove`` takes care of overlap issues).

    Note that the assignment of a scalar to an array is permitted. In addition,
    the library has to handle numeric-type conversion and for strings, padding and
    different character kinds.

.. index:: Coarray, _gfortran_caf_send_by_ref

.. _gfortran_caf_send_by_ref:

_gfortran_caf_send_by_ref --- Sending data from a local image to a remote image with enhanced referencing options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_send_by_ref (caf_token_t token, int image_index, gfc_descriptor_t *src, caf_reference_t *refs, int dst_kind, int src_kind, bool may_require_tmp, bool dst_reallocatable, int *stat, int dst_type)

  Called to send a scalar, an array section or a whole array from a local to a
  remote image identified by the :samp:`{image_index}`.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param src:
    intent(in) Array descriptor of the local array to be
    transferred to the remote image

  :param refs:
    intent(in) The references on the remote array to store
    the data given by src.  Guaranteed to have at least one entry.

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param dst_reallocatable:
    intent(in)  Set when the destination is of
    allocatable or pointer type and the refs will allow reallocation, i.e., the ref
    is a full array or component ref.

  :param stat:
    intent(out) When non- ``NULL`` give the result of the
    operation, i.e., zero on success and non-zero on error.  When ``NULL`` and
    an error occurs, then an error message is printed and the program is terminated.

  :param dst_type:
    intent(in)  Give the type of the destination.  When
    the destination is not an array, than the precise type, e.g. of a component in
    a derived type, is not known, but provided here.

  .. note::

    It is permitted to have :samp:`{image_index}` equal the current image; the memory of
    the send-to and the send-from might (partially) overlap in that case.  The
    implementation has to take care that it handles this case, e.g. using
    ``memmove`` which handles (partially) overlapping memory.  If
    :samp:`{may_require_tmp}` is true, the library might additionally create a
    temporary variable, unless additional checks show that this is not required
    (e.g. because walking backward is possible or because both arrays are
    contiguous and ``memmove`` takes care of overlap issues).

    Note that the assignment of a scalar to an array is permitted.  In addition,
    the library has to handle numeric-type conversion and for strings, padding
    and different character kinds.

    Because of the more complicated references possible some operations may be
    unsupported by certain libraries.  The library is expected to issue a precise
    error message why the operation is not permitted.

.. index:: Coarray, _gfortran_caf_get_by_ref

.. _gfortran_caf_get_by_ref:

_gfortran_caf_get_by_ref --- Getting data from a remote image using enhanced references
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_get_by_ref (caf_token_t token, int image_index, caf_reference_t *refs, gfc_descriptor_t *dst, int dst_kind, int src_kind, bool may_require_tmp, bool dst_reallocatable, int *stat, int src_type)

  Called to get a scalar, an array section or a whole array from a remote image
  identified by the :samp:`{image_index}`.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param refs:
    intent(in) The references to apply to the remote structure
    to get the data.

  :param dst:
    intent(in) Array descriptor of the local array to store
    the data transferred from the remote image.  May be reallocated where needed
    and when :samp:`{DST_REALLOCATABLE}` allows it.

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param dst_reallocatable:
    intent(in)  Set when :samp:`{DST}` is of
    allocatable or pointer type and its refs allow reallocation, i.e., the full
    array or a component is referenced.

  :param stat:
    intent(out) When non- ``NULL`` give the result of the
    operation, i.e., zero on success and non-zero on error.  When ``NULL`` and an
    error occurs, then an error message is printed and the program is terminated.

  :param src_type:
    intent(in)  Give the type of the source.  When the
    source is not an array, than the precise type, e.g. of a component in a
    derived type, is not known, but provided here.

  .. note::

    It is permitted to have ``image_index`` equal the current image; the memory
    of the send-to and the send-from might (partially) overlap in that case.  The
    implementation has to take care that it handles this case, e.g. using
    ``memmove`` which handles (partially) overlapping memory.  If
    :samp:`{may_require_tmp}` is true, the library might additionally create a
    temporary variable, unless additional checks show that this is not required
    (e.g. because walking backward is possible or because both arrays are
    contiguous and ``memmove`` takes care of overlap issues).

    Note that the library has to handle numeric-type conversion and for strings,
    padding and different character kinds.

    Because of the more complicated references possible some operations may be
    unsupported by certain libraries.  The library is expected to issue a precise
    error message why the operation is not permitted.

.. index:: Coarray, _gfortran_caf_sendget_by_ref

.. _gfortran_caf_sendget_by_ref:

_gfortran_caf_sendget_by_ref --- Sending data between remote images using enhanced references on both sides
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_sendget_by_ref (caf_token_t dst_token, int dst_image_index, caf_reference_t *dst_refs, caf_token_t src_token, int src_image_index, caf_reference_t *src_refs, int dst_kind, int src_kind, bool may_require_tmp, int *dst_stat, int *src_stat, int dst_type, int src_type)

  Called to send a scalar, an array section or a whole array from a remote image
  identified by the :samp:`{src_image_index}` to a remote image identified by the
  :samp:`{dst_image_index}`.

  :param dst_token:
    intent(in)  An opaque pointer identifying the
    destination coarray.

  :param dst_image_index:
    intent(in)  The ID of the destination remote
    image; must be a positive number.

  :param dst_refs:
    intent(in) The references on the remote array to store
    the data given by the source.  Guaranteed to have at least one entry.

  :param src_token:
    intent(in)  An opaque pointer identifying the source
    coarray.

  :param src_image_index:
    intent(in)  The ID of the source remote image;
    must be a positive number.

  :param src_refs:
    intent(in) The references to apply to the remote
    structure to get the data.

  :param dst_kind:
    intent(in)  Kind of the destination argument

  :param src_kind:
    intent(in)  Kind of the source argument

  :param may_require_tmp:
    intent(in)  The variable is ``false`` when
    it is known at compile time that the :samp:`{dest}` and :samp:`{src}` either cannot
    overlap or overlap (fully or partially) such that walking :samp:`{src}` and
    :samp:`{dest}` in element wise element order (honoring the stride value) will not
    lead to wrong results.  Otherwise, the value is ``true``.

  :param dst_stat:
    intent(out) when non- ``NULL`` give the result of
    the send-operation, i.e., zero on success and non-zero on error.  When
    ``NULL`` and an error occurs, then an error message is printed and the
    program is terminated.

  :param src_stat:
    intent(out) When non- ``NULL`` give the result of
    the get-operation, i.e., zero on success and non-zero on error.  When
    ``NULL`` and an error occurs, then an error message is printed and the
    program is terminated.

  :param dst_type:
    intent(in)  Give the type of the destination.  When
    the destination is not an array, than the precise type, e.g. of a component in
    a derived type, is not known, but provided here.

  :param src_type:
    intent(in)  Give the type of the source.  When the
    source is not an array, than the precise type, e.g. of a component in a
    derived type, is not known, but provided here.

  .. note::

    It is permitted to have the same image index for both :samp:`{src_image_index}` and
    :samp:`{dst_image_index}` ; the memory of the send-to and the send-from might
    (partially) overlap in that case.  The implementation has to take care that it
    handles this case, e.g. using ``memmove`` which handles (partially)
    overlapping memory.  If :samp:`{may_require_tmp}` is true, the library
    might additionally create a temporary variable, unless additional checks show
    that this is not required (e.g. because walking backward is possible or because
    both arrays are contiguous and ``memmove`` takes care of overlap issues).

    Note that the assignment of a scalar to an array is permitted.  In addition,
    the library has to handle numeric-type conversion and for strings, padding and
    different character kinds.

    Because of the more complicated references possible some operations may be
    unsupported by certain libraries.  The library is expected to issue a precise
    error message why the operation is not permitted.

.. index:: Coarray, _gfortran_caf_lock

.. _gfortran_caf_lock:

_gfortran_caf_lock --- Locking a lock variable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_lock (caf_token_t token, size_t index, int image_index, int *acquired_lock, int *stat, char *errmsg, size_t errmsg_len)

  Acquire a lock on the given image on a scalar locking variable or for the
  given array element for an array-valued variable.  If the :samp:`{acquired_lock}`
  is ``NULL``, the function returns after having obtained the lock.  If it is
  non- ``NULL``, then :samp:`{acquired_lock}` is assigned the value true (one) when
  the lock could be obtained and false (zero) otherwise.  Locking a lock variable
  which has already been locked by the same image is an error.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param index:
    intent(in)  Array index; first array index is 0.  For
    scalars, it is always 0.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param acquired_lock:
    intent(out) If not NULL, it returns whether lock
    could be obtained.

  :param stat:
    intent(out) Stores the STAT=; may be NULL.

  :param errmsg:
    intent(out) When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    This function is also called for critical blocks; for those, the array index
    is always zero and the image index is one.  Libraries are permitted to use other
    images for critical-block locking variables.

.. index:: Coarray, _gfortran_caf_unlock

.. _gfortran_caf_unlock:

_gfortran_caf_lock --- Unlocking a lock variable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_unlock (caf_token_t token, size_t index, int image_index, int *stat, char *errmsg, size_t errmsg_len)

  Release a lock on the given image on a scalar locking variable or for the
  given array element for an array-valued variable. Unlocking a lock variable
  which is unlocked or has been locked by a different image is an error.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param index:
    intent(in)  Array index; first array index is 0.  For
    scalars, it is always 0.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number.

  :param stat:
    intent(out) For allocatable coarrays, stores the STAT=;
    may be NULL.

  :param errmsg:
    intent(out) When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    This function is also called for critical block; for those, the array index
    is always zero and the image index is one.  Libraries are permitted to use other
    images for critical-block locking variables.

.. index:: Coarray, _gfortran_caf_event_post

.. _gfortran_caf_event_post:

_gfortran_caf_event_post --- Post an event
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_event_post (caf_token_t token, size_t index, int image_index, int *stat, char *errmsg, size_t errmsg_len)

  Increment the event count of the specified event variable.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param index:
    intent(in)  Array index; first array index is 0.  For
    scalars, it is always 0.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image, when accessed noncoindexed.

  :param stat:
    intent(out)  Stores the STAT=; may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    This acts like an atomic add of one to the remote image's event variable.
    The statement is an image-control statement but does not imply sync memory.
    Still, all preceeding push communications of this image to the specified
    remote image have to be completed before ``event_wait`` on the remote
    image returns.

.. index:: Coarray, _gfortran_caf_event_wait

.. _gfortran_caf_event_wait:

_gfortran_caf_event_wait --- Wait that an event occurred
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_event_wait (caf_token_t token, size_t index, int until_count, int *stat, char *errmsg, size_t errmsg_len)

  Wait until the event count has reached at least the specified
  :samp:`{until_count}` ; if so, atomically decrement the event variable by this
  amount and return.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param index:
    intent(in)  Array index; first array index is 0.  For
    scalars, it is always 0.

  :param until_count:
    intent(in)  The number of events which have to be
    available before the function returns.

  :param stat:
    intent(out)  Stores the STAT=; may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    This function only operates on a local coarray. It acts like a loop checking
    atomically the value of the event variable, breaking if the value is greater
    or equal the requested number of counts. Before the function returns, the
    event variable has to be decremented by the requested :samp:`{until_count}` value.
    A possible implementation would be a busy loop for a certain number of spins
    (possibly depending on the number of threads relative to the number of available
    cores) followed by another waiting strategy such as a sleeping wait (possibly
    with an increasing number of sleep time) or, if possible, a futex wait.

    The statement is an image-control statement but does not imply sync memory.
    Still, all preceeding push communications of this image to the specified
    remote image have to be completed before ``event_wait`` on the remote
    image returns.

.. index:: Coarray, _gfortran_caf_event_query

.. _gfortran_caf_event_query:

_gfortran_caf_event_query --- Query event count
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_event_query (caf_token_t token, size_t index, int image_index, int *count, int *stat)

  Return the event count of the specified event variable.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param index:
    intent(in)  Array index; first array index is 0.  For
    scalars, it is always 0.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image when accessed noncoindexed.

  :param count:
    intent(out)  The number of events currently posted to
    the event variable.

  :param stat:
    intent(out)  Stores the STAT=; may be NULL.

  .. note::

    The typical use is to check the local event variable to only call
    ``event_wait`` when the data is available. However, a coindexed variable
    is permitted; there is no ordering or synchronization implied.  It acts like
    an atomic fetch of the value of the event variable.

.. index:: Coarray, _gfortran_caf_sync_all

.. _gfortran_caf_sync_all:

_gfortran_caf_sync_all --- All-image barrier
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_sync_all (int *stat, char *errmsg, size_t errmsg_len)

  Synchronization of all images in the current team; the program only continues
  on a given image after this function has been called on all images of the
  current team.  Additionally, it ensures that all pending data transfers of
  previous segment have completed.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

.. index:: Coarray, _gfortran_caf_sync_images

.. _gfortran_caf_sync_images:

_gfortran_caf_sync_images --- Barrier for selected images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_sync_images (int count, int images[], int *stat, char *errmsg, size_t errmsg_len)

  Synchronization between the specified images; the program only continues on a
  given image after this function has been called on all images specified for
  that image. Note that one image can wait for all other images in the current
  team (e.g. via ``sync images(*)``) while those only wait for that specific
  image.  Additionally, ``sync images`` ensures that all pending data
  transfers of previous segments have completed.

  :param count:
    intent(in)  The number of images which are provided in
    the next argument.  For a zero-sized array, the value is zero.  For
    ``sync images (*)``, the value is -1.

  :param images:
    intent(in)  An array with the images provided by the
    user.  If :samp:`{count}` is zero, a NULL pointer is passed.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

.. index:: Coarray, _gfortran_caf_sync_memory

.. _gfortran_caf_sync_memory:

_gfortran_caf_sync_memory --- Wait for completion of segment-memory operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_sync_memory (int *stat, char *errmsg, size_t errmsg_len)

  Acts as optimization barrier between different segments. It also ensures that
  all pending memory operations of this image have been completed.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    A simple implementation could be
    ``__asm__ __volatile__ ("":::"memory")`` to prevent code movements.

.. index:: Coarray, _gfortran_caf_error_stop

.. _gfortran_caf_error_stop:

_gfortran_caf_error_stop --- Error termination with exit code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_error_stop (int error)

  Invoked for an ``ERROR STOP`` statement which has an integer argument.  The
  function should terminate the program with the specified exit code.

  :param error:
    intent(in)  The exit status to be used.

.. index:: Coarray, _gfortran_caf_error_stop_str

.. _gfortran_caf_error_stop_str:

_gfortran_caf_error_stop_str --- Error termination with string
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_error_stop (const char *string, size_t len)

  Invoked for an ``ERROR STOP`` statement which has a string as argument.  The
  function should terminate the program with a nonzero-exit code.

  :param string:
    intent(in)  the error message (not zero terminated)

  :param len:
    intent(in)  the length of the string

.. index:: Coarray, _gfortran_caf_fail_image

.. _gfortran_caf_fail_image:

_gfortran_caf_fail_image --- Mark the image failed and end its execution
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_fail_image ()

  Invoked for an ``FAIL IMAGE`` statement.  The function should terminate the
  current image.

  .. note::

    This function follows TS18508.

.. index:: Coarray, _gfortran_caf_atomic_define

.. _gfortran_caf_atomic_define:

_gfortran_caf_atomic_define --- Atomic variable assignment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_atomic_define (caf_token_t token, size_t offset, int image_index, void *value, int *stat, int type, int kind)

  Assign atomically a value to an integer or logical variable.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image when used noncoindexed.

  :param value:
    intent(in)  the value to be assigned, passed by reference

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param type:
    intent(in)  The data type, i.e. ``BT_INTEGER`` (1) or
    ``BT_LOGICAL`` (2).

  :param kind:
    intent(in)  The kind value (only 4; always ``int``)

.. index:: Coarray, _gfortran_caf_atomic_ref

.. _gfortran_caf_atomic_ref:

_gfortran_caf_atomic_ref --- Atomic variable reference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_atomic_ref (caf_token_t token, size_t offset, int image_index, void *value, int *stat, int type, int kind)

  Reference atomically a value of a kind-4 integer or logical variable.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image when used noncoindexed.

  :param value:
    intent(out)  The variable assigned the atomically
    referenced variable.

  :param stat:
    intent(out) Stores the status STAT= and may be NULL.

  :param type:
    the data type, i.e. ``BT_INTEGER`` (1) or
    ``BT_LOGICAL`` (2).

  :param kind:
    The kind value (only 4; always ``int``)

.. index:: Coarray, _gfortran_caf_atomic_cas

.. _gfortran_caf_atomic_cas:

_gfortran_caf_atomic_cas --- Atomic compare and swap
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_atomic_cas (caf_token_t token, size_t offset, int image_index, void *old, void *compare, void *new_val, int *stat, int type, int kind)

  Atomic compare and swap of a kind-4 integer or logical variable. Assigns
  atomically the specified value to the atomic variable, if the latter has
  the value specified by the passed condition value.

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image when used noncoindexed.

  :param old:
    intent(out)  The value which the atomic variable had
    just before the cas operation.

  :param compare:
    intent(in)  The value used for comparision.

  :param new_val:
    intent(in)  The new value for the atomic variable,
    assigned to the atomic variable, if ``compare`` equals the value of the
    atomic variable.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param type:
    intent(in)  the data type, i.e. ``BT_INTEGER`` (1) or
    ``BT_LOGICAL`` (2).

  :param kind:
    intent(in)  The kind value (only 4; always ``int``)

.. index:: Coarray, _gfortran_caf_atomic_op

.. _gfortran_caf_atomic_op:

_gfortran_caf_atomic_op --- Atomic operation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_atomic_op (int op, caf_token_t token, size_t offset, int image_index, void *value, void *old, int *stat, int type, int kind)

  Apply an operation atomically to an atomic integer or logical variable.
  After the operation, :samp:`{old}` contains the value just before the operation,
  which, respectively, adds (GFC_CAF_ATOMIC_ADD) atomically the ``value`` to
  the atomic integer variable or does a bitwise AND, OR or exclusive OR
  between the atomic variable and :samp:`{value}` ; the result is then stored in the
  atomic variable.

  :param op:
    intent(in)  the operation to be performed; possible values
    ``GFC_CAF_ATOMIC_ADD`` (1), ``GFC_CAF_ATOMIC_AND`` (2),
    ``GFC_CAF_ATOMIC_OR`` (3), ``GFC_CAF_ATOMIC_XOR`` (4).

  :param token:
    intent(in)  An opaque pointer identifying the coarray.

  :param offset:
    intent(in)  By which amount of bytes the actual data is
    shifted compared to the base address of the coarray.

  :param image_index:
    intent(in)  The ID of the remote image; must be a
    positive number; zero indicates the current image when used noncoindexed.

  :param old:
    intent(out)  The value which the atomic variable had
    just before the atomic operation.

  :param val:
    intent(in)  The new value for the atomic variable,
    assigned to the atomic variable, if ``compare`` equals the value of the
    atomic variable.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param type:
    intent(in)  the data type, i.e. ``BT_INTEGER`` (1) or
    ``BT_LOGICAL`` (2)

  :param kind:
    intent(in)  the kind value (only 4; always ``int``)

.. index:: Coarray, _gfortran_caf_co_broadcast

.. _gfortran_caf_co_broadcast:

_gfortran_caf_co_broadcast --- Sending data to all images
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_co_broadcast (gfc_descriptor_t *a, int source_image, int *stat, char *errmsg, size_t errmsg_len)

  Distribute a value from a given image to all other images in the team. Has to
  be called collectively.

  :param a:
    intent(inout)  An array descriptor with the data to be
    broadcasted (on :samp:`{source_image}`) or to be received (other images).

  :param source_image:
    intent(in)  The ID of the image from which the
    data should be broadcasted.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg.

.. index:: Coarray, _gfortran_caf_co_max

.. _gfortran_caf_co_max:

_gfortran_caf_co_max --- Collective maximum reduction
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_co_max (gfc_descriptor_t *a, int result_image, int *stat, char *errmsg, int a_len, size_t errmsg_len)

  Calculates for each array element of the variable :samp:`{a}` the maximum
  value for that element in the current team; if :samp:`{result_image}` has the
  value 0, the result shall be stored on all images, otherwise, only on the
  specified image. This function operates on numeric values and character
  strings.

  :param a:
    intent(inout)  An array descriptor for the data to be
    processed.  On the destination image(s) the result overwrites the old content.

  :param result_image:
    intent(in)  The ID of the image to which the
    reduced value should be copied to; if zero, it has to be copied to all images.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param a_len:
    intent(in)  the string length of argument :samp:`{a}`

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    If :samp:`{result_image}` is nonzero, the data in the array descriptor :samp:`{a}` on
    all images except of the specified one become undefined; hence, the library may
    make use of this.

.. index:: Coarray, _gfortran_caf_co_min

.. _gfortran_caf_co_min:

_gfortran_caf_co_min --- Collective minimum reduction
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_co_min (gfc_descriptor_t *a, int result_image, int *stat, char *errmsg, int a_len, size_t errmsg_len)

  Calculates for each array element of the variable :samp:`{a}` the minimum
  value for that element in the current team; if :samp:`{result_image}` has the
  value 0, the result shall be stored on all images, otherwise, only on the
  specified image. This function operates on numeric values and character
  strings.

  :param a:
    intent(inout)  An array descriptor for the data to be
    processed.  On the destination image(s) the result overwrites the old content.

  :param result_image:
    intent(in)  The ID of the image to which the
    reduced value should be copied to; if zero, it has to be copied to all images.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param a_len:
    intent(in)  the string length of argument :samp:`{a}`

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    If :samp:`{result_image}` is nonzero, the data in the array descriptor :samp:`{a}` on
    all images except of the specified one become undefined; hence, the library may
    make use of this.

.. index:: Coarray, _gfortran_caf_co_sum

.. _gfortran_caf_co_sum:

_gfortran_caf_co_sum --- Collective summing reduction
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_co_sum (gfc_descriptor_t *a, int result_image, int *stat, char *errmsg, size_t errmsg_len)

  Calculates for each array element of the variable :samp:`{a}` the sum of all
  values for that element in the current team; if :samp:`{result_image}` has the
  value 0, the result shall be stored on all images, otherwise, only on the
  specified image.  This function operates on numeric values only.

  :param a:
    intent(inout)  An array descriptor with the data to be
    processed.  On the destination image(s) the result overwrites the old content.

  :param result_image:
    intent(in)  The ID of the image to which the
    reduced value should be copied to; if zero, it has to be copied to all images.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    If :samp:`{result_image}` is nonzero, the data in the array descriptor :samp:`{a}` on
    all images except of the specified one become undefined; hence, the library may
    make use of this.

.. index:: Coarray, _gfortran_caf_co_reduce

.. _gfortran_caf_co_reduce:

_gfortran_caf_co_reduce --- Generic collective reduction
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_caf_co_reduce (gfc_descriptor_t *a, void * (*opr) (void *, void *), int opr_flags, int result_image, int *stat, char *errmsg, int a_len, size_t errmsg_len)

  Calculates for each array element of the variable :samp:`{a}` the reduction
  value for that element in the current team; if :samp:`{result_image}` has the
  value 0, the result shall be stored on all images, otherwise, only on the
  specified image.  The :samp:`{opr}` is a pure function doing a mathematically
  commutative and associative operation.

  :param a:
    intent(inout)  An array descriptor with the data to be
    processed.  On the destination image(s) the result overwrites the old content.

  :param opr:
    intent(in)  Function pointer to the reduction function

  :param opr_flags:
    intent(in)  Flags regarding the reduction function

  :param result_image:
    intent(in)  The ID of the image to which the
    reduced value should be copied to; if zero, it has to be copied to all images.

  :param stat:
    intent(out)  Stores the status STAT= and may be NULL.

  :param errmsg:
    intent(out)  When an error occurs, this will be set to
    an error message; may be NULL.

  :param a_len:
    intent(in)  the string length of argument :samp:`{a}`

  :param errmsg_len:
    intent(in)  the buffer size of errmsg

  .. note::

    If :samp:`{result_image}` is nonzero, the data in the array descriptor :samp:`{a}` on
    all images except of the specified one become undefined; hence, the library may
    make use of this.

    For character arguments, the result is passed as first argument, followed
    by the result string length, next come the two string arguments, followed
    by the two hidden string length arguments.  With C binding, there are no hidden
    arguments and by-reference passing and either only a single character is passed
    or an array descriptor.

.. Intrinsic Procedures
   -

Some basic guidelines for editing this document:

(1) The intrinsic procedures are to be listed in alphabetical order.
(2) The generic name is to be used.
(3) The specific names are included in the function index and in a
    table at the end of the node (See ABS entry).
(4) Try to maintain the same style for each entry.
