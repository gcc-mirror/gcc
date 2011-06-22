/* { dg-do run }  */
/* { dg-require-effective-target tls  }  */
/* { dg-require-effective-target pthread } */
/* { dg-options "-pthread" } */

#include <pthread.h>
extern int printf (char *,...);
__thread int a = 5; 
int *volatile a_in_other_thread = (int *) 12345;

static void *
thread_func (void *arg)
{
  a_in_other_thread = &a;
  a+=5;
  *((int *) arg) = a;
  return (void *)0;
}

int
main ()
{
  pthread_t thread;
  void *thread_retval;
  int *volatile a_in_main_thread;
  int *volatile again ;
  int thr_a;

  a_in_main_thread = &a;

  if (pthread_create (&thread, (pthread_attr_t *)0, thread_func, &thr_a))
    return 0;

  if (pthread_join (thread, &thread_retval))
    return 0;

  again = &a;
  if (again != a_in_main_thread)
    {
      printf ("FAIL: main thread addy changed from 0x%0x to 0x%0x\n", 
		a_in_other_thread, again);
      return 1;
    }

  if (a != 5 || thr_a != 10 || (a_in_other_thread == a_in_main_thread))
    {
      printf ("FAIL: a= %d, thr_a = %d Addr = 0x%0x\n", 
		a, thr_a, a_in_other_thread);
      return 1;
    }
  return 0;
}
