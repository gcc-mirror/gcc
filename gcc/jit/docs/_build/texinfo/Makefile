# Makefile for Sphinx Texinfo output

infodir ?= /usr/share/info

MAKEINFO = makeinfo --no-split
MAKEINFO_html = makeinfo --no-split --html
MAKEINFO_plaintext = makeinfo --no-split --plaintext
TEXI2PDF = texi2pdf --batch --expand
INSTALL_INFO = install-info

ALLDOCS = $(basename $(wildcard *.texi))

all: info
info: $(addsuffix .info,$(ALLDOCS))
plaintext: $(addsuffix .txt,$(ALLDOCS))
html: $(addsuffix .html,$(ALLDOCS))
pdf: $(addsuffix .pdf,$(ALLDOCS))

install-info: info
	for f in *.info; do \
	  cp -t $(infodir) "$$f" && \
	  $(INSTALL_INFO) --info-dir=$(infodir) "$$f" ; \
	done

uninstall-info: info
	for f in *.info; do \
	  rm -f "$(infodir)/$$f"  ; \
	  $(INSTALL_INFO) --delete --info-dir=$(infodir) "$$f" ; \
	done

%.info: %.texi
	$(MAKEINFO) -o '$@' '$<'

%.txt: %.texi
	$(MAKEINFO_plaintext) -o '$@' '$<'

%.html: %.texi
	$(MAKEINFO_html) -o '$@' '$<'

%.pdf: %.texi
	-$(TEXI2PDF) '$<'
	-$(TEXI2PDF) '$<'
	-$(TEXI2PDF) '$<'

clean:
	rm -f *.info *.pdf *.txt *.html
	rm -f *.log *.ind *.aux *.toc *.syn *.idx *.out *.ilg *.pla *.ky *.pg
	rm -f *.vr *.tp *.fn *.fns *.def *.defs *.cp *.cps *.ge *.ges *.mo

.PHONY: all info plaintext html pdf install-info uninstall-info clean
