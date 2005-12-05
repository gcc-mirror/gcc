/* On Darwin, you have to have a definition of the function to link,
   even if later on it won't be present in some dylib.  (That is,
   you have to link with the latest version of the dylib.)  */
void wf1(void) { }
void wf6(void) { }
void wf9(void) { }
void wf10(void) { }
void wf11(void) { }
void wf12(void) { }
void wf13(void) { }
void wf14(void) { }

int wv1;
int wv6;
int wv9;
int wv10;
int wv11;
int wv12;
int wv13;
int wv14;
