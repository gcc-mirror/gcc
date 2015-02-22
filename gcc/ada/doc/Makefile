# Makefile for Sphinx documentation

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = DOC_NAME=$* sphinx-build
PAPER         =
BUILDDIR      = build
SOURCEDIR     = .

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) \
		  -c $(SOURCEDIR)/share \
		  -d $(BUILDDIR)/$*/doctrees \
		  $(SOURCEDIR)
DOC_LIST=gnat_rm gnat_ugn
FMT_LIST=html pdf txt info

.PHONY: help clean

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  DOC_NAME.html       to make standalone HTML files"
	@echo "  DOC_NAME.pdf        to make LaTeX files and run them through pdflatex"
	@echo "  DOC_NAME.txt        to make text files"
	@echo "  DOC_NAME.texinfo    to make Texinfo files"
	@echo "  DOC_NAME.info       to make info files"
	@echo "  DOC_NAME.all        to build DOC_NAME for all previous formats"
	@echo "  all                 to build all documentations in all formats"
	@echo "  html-all            same as previous rule but only for HTML format"
	@echo "  pdf-all             same as previous rule but only for PDF format"
	@echo "  txt-all             same as previous rule but only for text format"
	@echo "  texinfo-all         same as previous rule but only for texinfo format"
	@echo "  info-all            same as previous rule but only for info format"
	@echo ""
	@echo "DOC_NAME should be a documentation name in the following list:"
	@echo "  $(DOC_LIST)"
	@echo ""
	@echo "source and location can be overriden using SOURCEDIR and BUILDDIR variables"

clean:
	-rm -rf $(BUILDDIR)/*/html \
		$(BUILDDIR)/*/pdf \
		$(BUILDDIR)/*/txt \
		$(BUILDDIR)/*/info

%.html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/$*/html

%.pdf:
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/$*/pdf
	cp $(SOURCEDIR)/share/sphinx.sty $(BUILDDIR)/$*/pdf
	$(MAKE) -C $(BUILDDIR)/$*/pdf all-pdf LATEXOPTS="-interaction=nonstopmode"

%.txt:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/txt
	$(MAKE) -C $(BUILDDIR)/$*/txt plaintext

%.info:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/info
	$(MAKE) -C $(BUILDDIR)/$*/info info

%.texinfo:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) $(BUILDDIR)/$*/texinfo
	sed -e 's/^@dircategory/@dircategory GNU Ada Tools/g' < $(BUILDDIR)/$*/texinfo/$*.texi > $(BUILDDIR)/../../$*.texi

html-all: $(foreach doc, $(DOC_LIST), $(doc).html)

pdf-all: $(foreach doc, $(DOC_LIST), $(doc).pdf)

txt-all: $(foreach doc, $(DOC_LIST), $(doc).txt)

texinfo-all: $(foreach doc, $(DOC_LIST), $(doc).texinfo)

%.all:
	$(MAKE) $(foreach fmt, $(FMT_LIST), $*.$(fmt))

all: $(foreach fmt, $(FMT_LIST), $(fmt).all)

