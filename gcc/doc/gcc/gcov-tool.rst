..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: gcov-tool

.. _gcov-tool:

gcov-tool---an Offline Gcda Profile Processing Tool
---------------------------------------------------

.. only:: man

  Synopsis
  ^^^^^^^^

  gcov-tool [ :option:`-v` | :option:`--version` ] [ :option:`-h` | :option:`--help` ]

  gcov-tool merge [merge-options] :samp:`{directory1}` :samp:`{directory2}`
        [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
        [ :option:`-v` | :option:`--verbose` ]
        [ :option:`-w` | :option:`--weight` :samp:`{w1,w2}` ]

  gcov-tool merge-stream [merge-stream-options] [ :samp:`{file}` ]
      [ :option:`-v` | :option:`--verbose` ]
      [ :option:`-w` | :option:`--weight` :samp:`{w1,w2}` ]

  gcov-tool rewrite [rewrite-options] :samp:`{directory}`
        [ :option:`-n` | :option:`--normalize` :samp:`{long_long_value}` ]
        [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
        [ :option:`-s` | :option:`--scale` :samp:`{float_or_simple-frac_value}` ]
        [ :option:`-v` | :option:`--verbose` ]

  gcov-tool overlap [overlap-options] :samp:`{directory1}` :samp:`{directory2}`
        [ :option:`-f` | :option:`--function` ]
        [ :option:`-F` | :option:`--fullname` ]
        [ :option:`-h` | :option:`--hotonly` ]
        [ :option:`-o` | :option:`--object` ]
        [ :option:`-t` | :option:`--hot_threshold` ] :samp:`{float}`
        [ :option:`-v` | :option:`--verbose` ]

.. only:: not man

  .. code-block::

    gcov-tool [global-options] SUB_COMMAND [sub_command-options] profile_dir

Description
^^^^^^^^^^^

:command:`gcov-tool` is an offline tool to process gcc's gcda profile files.

Current gcov-tool supports the following functionalities:

* merge two sets of profiles with weights.

* read a stream of profiles with associated filenames and merge it with a set of
  profiles with weights.

* read one set of profile and rewrite profile contents. One can scale or
  normalize the count values.

Examples of the use cases for this tool are:

* Collect the profiles for different set of inputs, and use this tool to merge
  them. One can specify the weight to factor in the relative importance of
  each input.

* Collect profiles from target systems without a filesystem (freestanding
  environments).  Merge the collected profiles with associated profiles
  present on the host system.  One can specify the weight to factor in the
  relative importance of each input.

* Rewrite the profile after removing a subset of the gcda files, while maintaining
  the consistency of the summary and the histogram.

* It can also be used to debug or libgcov code as the tools shares the majority
  code as the runtime library.

Note that for the merging operation, this profile generated offline may
contain slight different values from the online merged profile. Here are
a list of typical differences:

* histogram difference: This offline tool recomputes the histogram after merging
  the counters. The resulting histogram, therefore, is precise. The online
  merging does not have this capability -- the histogram is merged from two
  histograms and the result is an approximation.

* summary checksum difference: Summary checksum uses a CRC32 operation. The value
  depends on the link list order of gcov-info objects. This order is different in
  gcov-tool from that in the online merge. It's expected to have different
  summary checksums. It does not really matter as the compiler does not use this
  checksum anywhere.

* value profile counter values difference: Some counter values for value profile
  are runtime dependent, like heap addresses. It's normal to see some difference
  in these kind of counters.

Options
^^^^^^^

.. option:: -h, --help

  Display help about using :command:`gcov-tool` (on the standard output), and
  exit without doing any further processing.

.. option:: -v, --version

  Display the :command:`gcov-tool` version number (on the standard output),
  and exit without doing any further processing.

.. option:: merge

  Merge two profile directories.

  .. option:: -o directory, --output directory

    Set the output profile directory. Default output directory name is
    :samp:`{merged_profile}`.

  .. option:: -v, --verbose

    Set the verbose mode.

  .. option:: -w w1,w2, --weight w1,w2

    Set the merge weights of the :samp:`{directory1}` and :samp:`{directory2}`,
    respectively. The default weights are 1 for both.

.. option:: merge-stream

  Collect profiles with associated filenames from a *gcfn* and *gcda*
  data stream.  Read the stream from the file specified by :samp:`{file}` or from
  :samp:`stdin`.  Merge the profiles with associated profiles in the host
  filesystem.  Apply the optional weights while merging profiles.

  For the generation of a *gcfn* and *gcda* data stream on the target
  system, please have a look at the ``__gcov_filename_to_gcfn()`` and
  ``__gcov_info_to_gcda()`` functions declared in ``#include <gcov.h>``.

  .. option:: -v, --verbose

    Set the verbose mode.

  .. option:: -w w1,w2, --weight w1,w2

    Set the merge weights of the profiles from the *gcfn* and *gcda* data
    stream and the associated profiles in the host filesystem, respectively.  The
    default weights are 1 for both.

.. option:: rewrite

  Read the specified profile directory and rewrite to a new directory.

  .. option:: -n long_long_value, --normalize <long_long_value>

    Normalize the profile. The specified value is the max counter value
    in the new profile.

  .. option:: -o directory, --output directory

    Set the output profile directory. Default output name is :samp:`{rewrite_profile}`.

  .. option:: -s float_or_simple-frac_value, --scale float_or_simple-frac_value

    Scale the profile counters. The specified value can be in floating point value,
    or simple fraction value form, such 1, 2, 2/3, and 5/3.

  .. option:: -v, --verbose

    Set the verbose mode.

.. option:: overlap

  Compute the overlap score between the two specified profile directories.
  The overlap score is computed based on the arc profiles. It is defined as
  the sum of min (p1_counter[i] / p1_sum_all, p2_counter[i] / p2_sum_all),
  for all arc counter i, where p1_counter[i] and p2_counter[i] are two
  matched counters and p1_sum_all and p2_sum_all are the sum of counter
  values in profile 1 and profile 2, respectively.

  .. option:: -f, --function

    Print function level overlap score.

  .. option:: -F, --fullname

    Print full gcda filename.

  .. option:: -h, --hotonly

    Only print info for hot objects/functions.

  .. option:: -o, --object

    Print object level overlap score.

  .. option:: -t float, --hot_threshold <float>

    Set the threshold for hot counter value.

  .. option:: -v, --verbose

    Set the verbose mode.

.. only:: man

  .. include:: copyright.rst
