..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _final-install:

Final installation
------------------

Now that GCC has been built (and optionally tested), you can install it with

.. code-block:: bash

  cd objdir && make install

We strongly recommend to install into a target directory where there is
no previous version of GCC present.  Also, the GNAT runtime should not
be stripped, as this would break certain features of the debugger that
depend on this debugging information (catching Ada exceptions for
instance).

That step completes the installation of GCC; user level binaries can
be found in :samp:`{prefix}/bin` where :samp:`{prefix}` is the value
you specified with the :option:`--prefix` to configure (or
:samp:`/usr/local` by default).  (If you specified :option:`--bindir`,
that directory will be used instead; otherwise, if you specified
:option:`--exec-prefix`, :samp:`{exec-prefix}/bin` will be used.)
Headers for the C++ library are installed in
:samp:`{prefix}/include`; libraries in :samp:`{libdir}`
(normally :samp:`{prefix}/lib`); internal parts of the compiler in
:samp:`{libdir}/gcc` and :samp:`{libexecdir}/gcc`; documentation
in info format in :samp:`{infodir}` (normally
:samp:`{prefix}/info`).

When installing cross-compilers, GCC's executables
are not only installed into :samp:`{bindir}`, that
is, :samp:`{exec-prefix}/bin`, but additionally into
:samp:`{exec-prefix}/{target-alias}/bin`, if that directory
exists.  Typically, such :dfn:`tooldirs` hold target-specific
binutils, including assembler and linker.

Installation into a temporary staging area or into a :command:`chroot`
jail can be achieved with the command

.. code-block:: bash

  make DESTDIR=path-to-rootdir install

where :samp:`{path-to-rootdir}` is the absolute path of
a directory relative to which all installation paths will be
interpreted.  Note that the directory specified by ``DESTDIR``
need not exist yet; it will be created if necessary.

There is a subtle point with tooldirs and ``DESTDIR`` :
If you relocate a cross-compiler installation with
e.g. :samp:`DESTDIR={rootdir}`, then the directory
:samp:`{rootdir}/{exec-prefix}/{target-alias}/bin` will
be filled with duplicated GCC executables only if it already exists,
it will not be created otherwise.  This is regarded as a feature,
not as a bug, because it gives slightly more control to the packagers
using the ``DESTDIR`` feature.

You can install stripped programs and libraries with

.. code-block:: bash

  make install-strip

If you are bootstrapping a released version of GCC then please
quickly review the build status page for your release, available from
https://gcc.gnu.org/buildstat.html.
If your system is not listed for the version of GCC that you built,
send a note to
gcc@gcc.gnu.org indicating
that you successfully built and installed GCC.
Include the following information:

* Output from running :samp:`{srcdir}/config.guess`.  Do not send
  that file itself, just the one-line output from running it.

* The output of :samp:`gcc -v` for your newly installed :command:`gcc`.
  This tells us which version of GCC you built and the options you passed to
  configure.

* Whether you enabled all languages or a subset of them.  If you used a
  full distribution then this information is part of the configure
  options in the output of :samp:`gcc -v`, but if you downloaded the
  'core' compiler plus additional front ends then it isn't apparent
  which ones you built unless you tell us about it.

* If the build was for GNU/Linux, also include:

  * The distribution name and version (e.g., Red Hat 7.1 or Debian 2.2.3);
    this information should be available from :samp:`/etc/issue`.

  * The version of the Linux kernel, available from :samp:`uname --version`
    or :samp:`uname -a`.

  * The version of glibc you used; for RPM-based systems like Red Hat,
    Mandrake, and SuSE type :samp:`rpm -q glibc` to get the glibc version,
    and on systems like Debian and Progeny use :samp:`dpkg -l libc6`.

  For other systems, you can include similar information if you think it is
  relevant.

* Any other information that you think would be useful to people building
  GCC on the same configuration.  The new entry in the build status list
  will include a link to the archived copy of your message.

We'd also like to know if the
:ref:`specific`
didn't include your host/target information or if that information is
incomplete or out of date.  Send a note to
gcc@gcc.gnu.org detailing how the information should be changed.

If you find a bug, please report it following the
`bug reporting guidelines <https://gcc.gnu.org/bugs/>`_.

If you want to print the GCC manuals, do :samp:`cd {objdir}; make pdf`
You will need to have Sphinx (version at least |needs_sphinx|)
and XeLaTex installed.
You can also `buy printed manuals from the
Free Software Foundation <https://shop.fsf.org/>`_, though such manuals may not be for the most
recent version of GCC.

If you would like to generate online HTML documentation, do :samp:`cd
{objdir}; make html`.
