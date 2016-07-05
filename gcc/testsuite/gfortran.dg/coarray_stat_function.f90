! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
program function_stat
  implicit none

  integer :: me[*],tmp,stat,stat2,next

  me = this_image()
  next = me + 1
  if(me == num_images()) next = 1
  stat = 0

  sync all(stat=stat)

  if(stat /= 0) write(*,*) 'Image failed during sync'

  stat = 0
  if(me == 1) then
     tmp = func(me[4,stat=stat])
     if(stat /= 0) write(*,*) me,'failure in func arg'
   else if(me == 2) then
      tmp = func2(me[1,stat=stat2],me[3,stat=stat])
      if(stat2 /= 0 .or. stat /= 0) write(*,*) me,'failure in func2 args'
  endif

contains

  function func(remote_me)
    integer func
    integer remote_me
    func = remote_me
  end function func

  function func2(remote_me,remote_neighbor)
    integer func2
    integer remote_me,remote_neighbor
    func2 = remote_me + remote_neighbor
  end function func2
  
end program function_stat

! { dg-final { scan-tree-dump-times "_gfortran_caf_get \\\(caf_token.0, \\\(integer\\\(kind=\[48\]\\\)\\\) desc.\[0-9\]+.data - \\\(integer\\\(kind=\[48\]\\\)\\\) me, 4, &desc.\[0-9\]+, 0B, &desc.\[0-9\]+, 4, 4, 0, &stat\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_get \\\(caf_token.0, \\\(integer\\\(kind=\[48\]\\\)\\\) desc.\[0-9\]+.data - \\\(integer\\\(kind=\[48\]\\\)\\\) me, 1, &desc.\[0-9\]+, 0B, &desc.\[0-9\]+, 4, 4, 0, &stat2\\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_get \\\(caf_token.0, \\\(integer\\\(kind=\[48\]\\\)\\\) desc.\[0-9\]+.data - \\\(integer\\\(kind=\[48\]\\\)\\\) me, 3, &desc.\[0-9\]+, 0B, &desc.\[0-9\]+, 4, 4, 0, &stat\\\);" 1 "original" } }
