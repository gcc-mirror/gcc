/* { dg-options "-O0 -pthread -fprofile-update=atomic" } */
#include <pthread.h>

#define NUM_THREADS	8
#define SIZE 1024
#define ITERATIONS (1000 * 1000)

char buffer[SIZE];
char buffer2[SIZE];

void *copy_memory(char *dst, char *src, unsigned size)
{
   for (unsigned i = 0; i < ITERATIONS; i++)
   {
     dst[size % 10] = src[size % 20];
   }
}

void *foo(void *d)
{
  copy_memory (buffer, buffer2, SIZE);
}

int main(int argc, char *argv[])
{
   pthread_t threads[NUM_THREADS];
   int rc;
   long t;
   for(t=0;t<NUM_THREADS;t++){
     rc = pthread_create(&threads[t], NULL, foo, 0);
     if (rc){
	 return 1;
       }
     }

   int retval;
   for(t=0;t<NUM_THREADS;t++)
     pthread_join (threads[t], (void**)&retval);

   return buffer[10];
}
