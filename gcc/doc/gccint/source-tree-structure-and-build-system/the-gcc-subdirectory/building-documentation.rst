..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _building_documentation:

Building Documentation
^^^^^^^^^^^^^^^^^^^^^^

The main GCC documentation uses `Sphinx`_ to generate pretty documentation
from `reStructuredText`_ format.
These are input for Manual pages format (:command:`make man`),
Info format (:command:`make info`), HTML format (:command:`make html`)
and PDF format (:command:`make pdf`).

.. _Sphinx: http://www.sphinx-doc.org/
.. _reStructuredText: http://docutils.sourceforge.net/rst.html

.. _sphinx_install:

Sphinx Install
==============

The ReST markups currently used by the documentation files are meant to be
built with ``Sphinx`` version |needs_sphinx| or higher.

Most distributions are shipped with Sphinx, but its toolchain is fragile,
and it is not uncommon that upgrading it or some other Python packages
on your machine would cause the documentation build to break.

A way to avoid that is to use a different version than the one shipped
with your distributions. In order to do so, it is recommended to install
Sphinx inside a virtual environment, using ``virtualenv-3``
or ``virtualenv``, depending on how your distribution packaged Python 3.

.. code-block:: shell-session

  $ virtualenv /tmp/venv
  $ source /tmp/venv/bin/activate
  $ pip install -r requirements.txt

Note the :file:`requirements.txt` file is placed in :rst:dir:`gcc/doc` folder and contains the following packages:

.. literalinclude:: ../../../../../doc/requirements.txt
  :caption: requirements.txt

Then the virtualenv can be provided to the configure script :option:`install:--with-sphinx-build`
and will be used by the build system:

.. code-block:: shell-session

  $ configure --with-sphinx-build=/tmp/venv/bin/sphinx-build

If you want to build the PDF documentation, you will need ``python3-Sphinx-latex`` sub-package.

.. note::

   When building **manual** and **info** pages (built by default), the only dependency
   is Sphinx package and one can ignore other dependencies mentioned in :file:`requirements.txt`.

Writing Documentation
=====================

Adding new documentation can be as simple as:

1. Add a new ``.rst`` file somewhere under a ``doc`` subfolder.
2. Refer to it from the Sphinx main `TOC tree`_ in a ``index.rst`` file.

.. _TOC tree: http://www.sphinx-doc.org/en/stable/markup/toctree.html

This is usually good enough for simple documentation (like the one you're
reading right now), but for larger documents it may be advisable to create a
subdirectory (or use an existing one).

See the documentation for `Sphinx`_ and `reStructuredText`_ on what you can do
with them. In particular, the Sphinx `reStructuredText Primer`_ is a good place
to get started with reStructuredText. There are also some `Sphinx specific
markup constructs`_ and `Usefull RST cheatsheet`_.

.. _reStructuredText Primer: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx specific markup constructs: http://www.sphinx-doc.org/en/stable/markup/index.html
.. _Usefull RST cheatsheet: https://github.com/ralsina/rst-cheatsheet/blob/master/rst-cheatsheet.rst

Specific guidelines for the GCC documentation
---------------------------------------------

Here are some specific guidelines for the GCC documentation:

* Please stick to this order of heading adornments:

  1. ``=`` with overline for document title:

    .. code-block:: rst

      ==============
      Document title
      ==============

  2. ``-`` for chapters:

    .. code-block:: rst

      Chapter
      -------

  3. ``*`` for sections:

    .. code-block:: rst

        Section
        *******

  4. ``^`` for subsections:

    .. code-block:: rst

        Subsection
        ^^^^^^^^^^

  5. ``~`` for subsubsections:

    .. code-block:: rst

        Subsubsection
        ~~~~~~~~~~~~~

  Although RST doesn't mandate a specific order ("Rather than imposing a fixed
  number and order of section title adornment styles, the order enforced will be
  the order as encountered."), having the higher levels the same overall makes
  it easier to follow the documents.

* For inserting fixed width text blocks (for code examples, use case
  examples, etc.), use ``::`` for anything that doesn't really benefit
  from syntax highlighting, especially short snippets. Use
  ``.. code-block:: <language>`` for longer code blocks that benefit
  from highlighting. For a short snippet of code embedded in the text, use ````code snippet````.

* For parts of documentation that needs to be written or enhanced, use ``.. todo::`` directive
  (part of the official ``sphinx.ext.todo`` extension). In development mode, all items
  are listed at the very end of the documentation for easier navigation.

GCC-specific directives and roles
---------------------------------

GCC uses its own extension (:file:`gcc_sphinx.py`) that defined various directives. For the complete
list of target-specific attributes, please take a look at the extension definition:

.. list-table::
   :header-rows: 1

   * - Directive
     - Description

   * - ``gcc-attr``
     - Generic GCC attribute
   * - ``fn-attr``
     - Function attribute
   * - ``var-attr``
     - Variable attribute
   * - ``type-attr``
     - Type attribute
   * - ``enum-attr``
     - Enumeral attribute
   * - ``label-attr``
     - Label attribute
   * - ``$target-fn-attr``
     - Target-specific function attribute (e.g. ``.. x86-fn-attr:: interrupt``)
   * - ``$target-var-attr``
     - Target-specific variable attribute
   * - ``$target-type-attr``
     - Target-specific type attribute
   * - ``gcc-param``
     - GCC parameter directive, (e.g. ``.. gcc-param: inline-unit-growth``)

Apart from the directives, we also define various inline roles:

.. list-table::
   :header-rows: 1

   * - Role
     - Description

   * - ``:P:`$num```
     - Link to WG21 - The C++ Standards Committee (e.g. ``:P:`2003```)
   * - ``:PR:`$num```
     - Link to GCC bugzilla entry
   * - ``:openmp:`$version```
     - Link to OpenMP documentation version ``$version`` (e.g. ``:openmp:`4.5```)
   * - ``:openacc:`$version```
     - Link to OpenACC documentation version ``$version``
   * - ``|gcc_version|``
     - Version of the documentation (e.g. |gcc_version|, taken from :file:`BASE-VER`)
   * - ``|needs_sphinx|``
     - Minimal required Sphinx version (e.g. |needs_sphinx|, taken from :file:`baseconf.py`)
   * - ``|bugurl|``
     - URL to bugzilla instance (e.g. |bugurl|)
   * - ``|package_version|``
     - Package version (e.g. |package_version|)
   * - ``|gol|``
     - Line break used for PDF version
   * - ``|nbsp|``
     - Non-breaking space character

.. _miscellaneous-docs:

Miscellaneous Documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In addition to the formal documentation that is installed by GCC,
there are several other text files in the :samp:`gcc` subdirectory
with miscellaneous documentation:

:samp:`ABOUT-GCC-NLS`
  Notes on GCC's Native Language Support.

  .. todo:: this should be part of this manual rather than a separate file

:samp:`ABOUT-NLS`
  Notes on the Free Translation Project.

:samp:`COPYING`
  The GNU General Public License, Versions 2 and 3.

:samp:`COPYING.LIB` :samp:`COPYING3.LIB`
  The GNU Lesser General Public License, Versions 2.1 and 3.

:samp:`*ChangeLog*` :samp:`*/ChangeLog*`
  Change log files for various parts of GCC.

:samp:`LANGUAGES`
  Details of a few changes to the GCC front-end interface.

  .. todo:: the information in this file should be part of general documentation of
    the front-end interface in this manual.

:samp:`ONEWS`
  Information about new features in old versions of GCC.  (For recent
  versions, the information is on the GCC web site.)

:samp:`README.Portability`
  Information about portability issues when writing code in GCC.

  .. todo:: why isn't this part of this manual or of the GCC Coding Conventions?

  .. todo:: document such files in subdirectories, at least :samp:`config`,
    :samp:`c`, :samp:`cp`, :samp:`objc`, :samp:`testsuite`.
