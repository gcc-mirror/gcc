! { dg-do run }
! pr18983
! could not write to /dev/null

#if defined  _WIN32
#define DEV_NULL "nul"
#else
#define DEV_NULL "/dev/null"
#endif

       integer i
       open(10,file=DEV_NULL)
       do i = 1,100
         write(10,*) "Hello, world"
       end do
       end
