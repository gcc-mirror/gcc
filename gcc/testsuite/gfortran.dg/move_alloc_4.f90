! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR 48700: memory leak with MOVE_ALLOC
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

program testmv3
  type bar
    integer, allocatable  :: ia(:), ja(:)
  end type

 block ! For auto-dealloc, as PROGRAM implies SAVE
  type(bar), allocatable :: sm,sm2

  allocate(sm)
  allocate(sm%ia(10),sm%ja(10))

  call move_alloc(sm2,sm)
 end block
end program testmv3 

! { dg-final { scan-tree-dump-times "__builtin_free" 9 "original" } }
