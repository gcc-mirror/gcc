#ifndef NO_LABEL_VALUES
x(a){static void*j[]={&&l1,&&l2};goto*j[a];l1:return 0;l2:return 1;}
#else
main(){ exit (0); }
#endif
