/* CYGNUS LOCAL -- meissner/no label values */
#ifndef NO_LABEL_VALUES
main(){__label__ l;void*x(){return&&l;}goto*x();abort();return;l:exit(0);}
#else
main(){ exit (0); }
#endif
/* END CYGNUS LOCAL -- meissner/no label values */
