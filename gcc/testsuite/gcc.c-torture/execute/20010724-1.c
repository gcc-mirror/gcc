/* Verify that the workarounds in config/mips/irix6-libc-compat.c are still
   needed.  */

/* IRIX 6, unlike other Unix systems, defines union semun in <sys/sem.h>.
   Inhibit this definition to be able to run this test on other platforms.  */
#define _XOPEN_SOURCE

#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/ipc.h>
#include <sys/sem.h>

union semun {
  int val;
  struct semid_ds *buf;
  ushort_t *array;
};

int
main (void)
{
  struct in_addr ia;
  int semid;
  union semun su;

  ia.s_addr = INADDR_BROADCAST;

  if (strcmp (inet_ntoa (ia), "255.255.255.255") != 0)
    abort ();

  ia.s_addr = INADDR_LOOPBACK;

  if (inet_lnaof (ia) != 1)
    abort ();

  if (inet_netof (ia) != IN_LOOPBACKNET)
    abort ();

  ia = inet_makeaddr (IN_LOOPBACKNET, 1);
  if (ia.s_addr != INADDR_LOOPBACK)
    abort ();

  if ((semid = semget (IPC_PRIVATE, 1, IPC_CREAT | IPC_EXCL | SEM_R | SEM_A)) < 0)
    abort ();

  su.val = 10;

  if (semctl (semid, 0, SETVAL, su) != 0)
    abort ();

  if (semctl (semid, 0, GETVAL) != 10)
    abort ();

  if (semctl (semid, 0, IPC_RMID) != 0)
    abort ();

  return 0;
}
