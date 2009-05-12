STAGE2_CFLAGS += -g0
do-compare = $(SHELL) $(srcdir)/contrib/compare-debug $$f1 $$f2
