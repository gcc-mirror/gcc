int a[2];
const int (&rc)[2] = a;
volatile int (&rv)[2] = a;
const volatile int (&rcv)[2] = a;
