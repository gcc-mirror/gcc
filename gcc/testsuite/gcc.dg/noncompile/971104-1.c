extern int printf(const char *, ...);
typedef unsigned short          ushort;
struct sembuf {
  ushort  sem_num;         
  short   sem_op;          
  short   sem_flg;         
};
union semun {
  int val;                       
  struct semid_ds *buf;          
  ushort *array;                 
  struct seminfo *__buf;         
  void *__pad;
};
static union semun semctl_arg;
static int semid;
static void up(int sem){
  struct sembuf sb;
  sb.sem_num = (unsigned short) sem;
  sb.sem_op  = 1;         
  sb.sem_flg = 0x1000 ;  
  if(semop(semid, &sb, 1) == -1) error("up failure");
  if(semctl(semid, sb.sem_num, 12 , semctl_arg) == 0)
    printf("%s had processes sleeping on it!\n",
    ({ "MUTEX     ", "BARB_SEM 1", "BARB_SEM 2", "CUST_SEM 1",
       "CUST_SEM 2", "WAIT_SEM 1", "WAIT_SEM 2", "WAIT_SEM 3",
       "WAIT_SEM 4"}	 /* { dg-error "parse error|syntax error|expected" } */
	[( sb.sem_num )]) );
}
