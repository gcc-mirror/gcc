#define XSTR(a,b) a ## b
#define T(a,b) XSTR(a,b)

int  T(NAME,ii)(int a, int b)   { return -(a CODE b); }
int  T(NAME,il)(long a, long b) { return -(a CODE b); }
long T(NAME,li)(int a, int b)   { return -(a CODE b); }
long T(NAME,ll)(long a, long b) { return -(a CODE b); }

int  T(NAME,iin0)(int a)  { return -(a CODE 0); }
int  T(NAME,iln0)(long a) { return -(a CODE 0); }
long T(NAME,lin0)(int a)  { return -(a CODE 0); }
long T(NAME,lln0)(long a) { return -(a CODE 0); }

int  T(NAME,iin1)(int a)  { return -(a CODE 1); }
int  T(NAME,iln1)(long a) { return -(a CODE 1); }
long T(NAME,lin1)(int a)  { return -(a CODE 1); }
long T(NAME,lln1)(long a) { return -(a CODE 1); }

int  T(NAME,iinm1)(int a)  { return -(a CODE -1); }
int  T(NAME,ilnm1)(long a) { return -(a CODE -1); }
long T(NAME,linm1)(int a)  { return -(a CODE -1); }
long T(NAME,llnm1)(long a) { return -(a CODE -1); }

int  T(NAME,iin42)(int a)  { return -(a CODE 42); }
int  T(NAME,iln42)(long a) { return -(a CODE 42); }
long T(NAME,lin42)(int a)  { return -(a CODE 42); }
long T(NAME,lln42)(long a) { return -(a CODE 42); }
