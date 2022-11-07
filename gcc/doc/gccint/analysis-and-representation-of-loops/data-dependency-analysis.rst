..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Data Dependency Analysis

.. _dependency-analysis:

Data Dependency Analysis
************************

The code for the data dependence analysis can be found in
:samp:`tree-data-ref.cc` and its interface and data structures are
described in :samp:`tree-data-ref.h`.  The function that computes the
data dependences for all the array and pointer references for a given
loop is ``compute_data_dependences_for_loop``.  This function is
currently used by the linear loop transform and the vectorization
passes.  Before calling this function, one has to allocate two vectors:
a first vector will contain the set of data references that are
contained in the analyzed loop body, and the second vector will contain
the dependence relations between the data references.  Thus if the
vector of data references is of size ``n``, the vector containing the
dependence relations will contain ``n*n`` elements.  However if the
analyzed loop contains side effects, such as calls that potentially can
interfere with the data references in the current analyzed loop, the
analysis stops while scanning the loop body for data references, and
inserts a single ``chrec_dont_know`` in the dependence relation
array.

The data references are discovered in a particular order during the
scanning of the loop body: the loop body is analyzed in execution order,
and the data references of each statement are pushed at the end of the
data reference array.  Two data references syntactically occur in the
program in the same order as in the array of data references.  This
syntactic order is important in some classical data dependence tests,
and mapping this order to the elements of this array avoids costly
queries to the loop body representation.

Three types of data references are currently handled: ARRAY_REF,
INDIRECT_REF and COMPONENT_REF. The data structure for the data reference
is ``data_reference``, where ``data_reference_p`` is a name of a
pointer to the data reference structure. The structure contains the
following elements:

* ``base_object_info`` : Provides information about the base object
  of the data reference and its access functions. These access functions
  represent the evolution of the data reference in the loop relative to
  its base, in keeping with the classical meaning of the data reference
  access function for the support of arrays. For example, for a reference
  ``a.b[i][j]``, the base object is ``a.b`` and the access functions,
  one for each array subscript, are:
  ``{i_init, + i_step}_1, {j_init, +, j_step}_2``.

* ``first_location_in_loop`` : Provides information about the first
  location accessed by the data reference in the loop and about the access
  function used to represent evolution relative to this location. This data
  is used to support pointers, and is not used for arrays (for which we
  have base objects). Pointer accesses are represented as a one-dimensional
  access that starts from the first location accessed in the loop. For
  example:

  .. code-block:: c++

          for1 i
             for2 j
              *((int *)p + i + j) = a[i][j];

  The access function of the pointer access is ``{0, + 4B}_for2``
  relative to ``p + i``. The access functions of the array are
  ``{i_init, + i_step}_for1`` and ``{j_init, +, j_step}_for2``
  relative to ``a``.

  Usually, the object the pointer refers to is either unknown, or we cannot
  prove that the access is confined to the boundaries of a certain object.

  Two data references can be compared only if at least one of these two
  representations has all its fields filled for both data references.

  The current strategy for data dependence tests is as follows:
  If both ``a`` and ``b`` are represented as arrays, compare
  ``a.base_object`` and ``b.base_object`` ;
  if they are equal, apply dependence tests (use access functions based on
  base_objects).
  Else if both ``a`` and ``b`` are represented as pointers, compare
  ``a.first_location`` and ``b.first_location`` ;
  if they are equal, apply dependence tests (use access functions based on
  first location).
  However, if ``a`` and ``b`` are represented differently, only try
  to prove that the bases are definitely different.

* Aliasing information.

* Alignment information.

The structure describing the relation between two data references is
``data_dependence_relation`` and the shorter name for a pointer to
such a structure is ``ddr_p``.  This structure contains:

* a pointer to each data reference,

* a tree node ``are_dependent`` that is set to ``chrec_known``
  if the analysis has proved that there is no dependence between these two
  data references, ``chrec_dont_know`` if the analysis was not able to
  determine any useful result and potentially there could exist a
  dependence between these data references, and ``are_dependent`` is
  set to ``NULL_TREE`` if there exist a dependence relation between the
  data references, and the description of this dependence relation is
  given in the ``subscripts``, ``dir_vects``, and ``dist_vects``
  arrays,

* a boolean that determines whether the dependence relation can be
  represented by a classical distance vector,

* an array ``subscripts`` that contains a description of each
  subscript of the data references.  Given two array accesses a
  subscript is the tuple composed of the access functions for a given
  dimension.  For example, given ``A[f1][f2][f3]`` and
  ``B[g1][g2][g3]``, there are three subscripts: ``(f1, g1), (f2,
  g2), (f3, g3)``.

* two arrays ``dir_vects`` and ``dist_vects`` that contain
  classical representations of the data dependences under the form of
  direction and distance dependence vectors,

* an array of loops ``loop_nest`` that contains the loops to
  which the distance and direction vectors refer to.

Several functions for pretty printing the information extracted by the
data dependence analysis are available: ``dump_ddrs`` prints with a
maximum verbosity the details of a data dependence relations array,
``dump_dist_dir_vectors`` prints only the classical distance and
direction vectors for a data dependence relations array, and
``dump_data_references`` prints the details of the data references
contained in a data reference array.