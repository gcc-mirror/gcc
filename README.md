Overview
========
This repository exists as a staging ground for developing candidate
contributions to the GNU Compiler Collection ([GCC]).

Candidate features currently on this site are listed below.  The checked items indicate features 
that have been submitted to the relevant GCC developer mailing list for review.  We welcome user 
feedback from testing these features.  Unchecked items are insufficiently mature to submit to GCC. 

- [X] The [teams branch] with experimental support for Fortran 2015 teams. 
- [X] The [download-opencoarrays-mpich branch] modifying GCC's prequisites download script
  to add [OpenCoarrays] and [MPICH] to the list of downloade software.
- [ ] The [issue-#6-iso-fortran-binding-h branch] providing the Fortran 2015 `iso_fortran_binding.h` file.

We welcome code contributions via [pull request] for any listed features or other unsupported
features of the [draft Fortran 2015 standard]:

Installation
============
This fork's [releases] are produced from the [teams branch], which we expect will be of 
most interest to users.   We recommend building the [latest release] using the [OpenCoarrays] 
`install.sh` script as follows:
```bash
# Install a teams-branch release of this GCC fork
git clone https://github.com/sourceryinstitute/opencoarrays
cd oppencoarrays
./install.sh --package gcc \
  --from-url https://github.com/sourceryinstitute/gcc/archive/<release-tag>.tar.gz \
  --install-version <release-tag>
```
where <release-tag> must be replaced with the appropriate string such as teams-20170921.  The final
command above peforms the safest but slowest build, which could take several hours to complete.
For a speedier build, add the `--disable-bootstrap`, which might fail if the GCC you are using
to build is too old.  Also, for an interactive build, add `--yes-to-all` to instruct the
installer to assume affirmative answers to any queries. If successful, the above steps will 
install GCC in the `prerequisites/installations` subdirectory.  To see additional installation
options, including choosing another installation path, execute `./install.sh --help`.

Next use the resulting compilers to build [MPICH] 3.2 or later:
```bash
#Install the teams-branch release of the GCC fork:
./install.sh --package gcc --from-url https://github.com/sourceryinstitute/gcc/teams-<release-tag>

# Install MPICH 3.2 or later using the GCC that was installed above:
export LD_LIBRARY_PATH="${PWD}"/prerequisites/installations/gcc/teams-<release-tag>/lib64

./install.sh --package mpich \
   --with-fortran "${PWD}"/prerequisites/installations/gcc/<release-tag>/bin/gfortarn \
   --with-c       "${PWD}"/prerequisites/installations/gcc/<release-tag>/bin/gcc      \
   --with-cpp     "${PWD}"/prerequisites/installations/gcc/<release-tag>bin/g++

```

Next use the resulting [MPICH] installation to build the [opencoarrays-teams branch] of OpenCoarrays.  
```bash
# Install OpenCoarrays using the MPICH just installed:
git checkout opencoarrays-teams
./install.sh  \
   --with-mpi "${PWD}"/prerequisites/installations/mpich/<mpich-version-number>/
```
after again replacing text between angular brackets with the appropriate string (<...>) such as "3.2".
Please report any problems with the above steps on our [issues page].


[GCC mirror] README
===================

This directory contains the GNU Compiler Collection (GCC).

The GNU Compiler Collection is free software.  See the files whose
names start with COPYING for copying permission.  The manuals, and
some of the runtime libraries, are under different terms; see the
individual source files for details.

The directory INSTALL contains copies of the installation information
as HTML and plain text.  The source of this information is
gcc/doc/install.texi.  The installation information includes details
of what is included in the GCC sources and what files GCC installs.

See the file gcc/doc/gcc.texi (together with other files that it
includes) for usage and porting information.  An online readable
version of the manual is in the files gcc/doc/gcc.info*.

See http://gcc.gnu.org/bugs/ for how to report bugs usefully.

Copyright years on GCC source files may be listed using range
notation, e.g., 1987-2012, indicating that every year in the range,
inclusive, is a copyrightable year that could otherwise be listed
individually.

[GCC mirror]: https://github.com/gcc-mirror/gcc
[GCC]: https://gcc.gnu.org/gcc
[OpenCoarrays]: https://www.opendcoarrays.org
[MPICH]: https://www.mpich.org
[teams branch]: https://github.com/sourceryinstitute/gcc/tree/teams
[issue-#6-iso-fortran-binding-h branch]: https://github.com/sourceryinstitute/gcc/tree/issue-#6-iso-fortran-binding-h
[download-opencoarrays-mpich branch]: https://github.com/sourceryinstitute/gcc/tree/download-opencoarrays-mpich 
[releases]: https://github.com/sourceryinstitute/gcc/releases/
[issues page]: https://github.com/sourceryinstitute/gcc/issues/
[opencoarrays-teams branch]: https://github.com/sourceryinstitute/opencoarrays/tree/opencoarrays-teams
[pull request]: https://github.com/sourceryinstitute/gcc/pulls
[draft Fortran 2015 standard]: https://bit.ly/fortran-2015-draft
[latest release]: https://github.com/sourceryinstitute/gcc/releases/latest
