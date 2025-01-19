! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
!
! PR fortran/104819 - passing of NULL() actual argument to character dummy

program p
  implicit none
  character(kind=1,len=10), pointer :: c     => NULL()
  character(kind=1,len=:),  pointer :: d     => NULL()
  character(kind=1,len=10), pointer :: c1(:) => NULL()
  character(kind=1,len=:),  pointer :: d1(:) => NULL()
  character(kind=4,len=10), pointer :: c4(:) => NULL()
  character(kind=4,len=:),  pointer :: d4(:) => NULL()

  ! scalar character variables:
  ! kind=1, assumed length
  call rank0_al (null(c))
  call rank0_al (c)
  call arank_al (null(c))
  call arank_al (c)
  call rank0_alb(null(c))
  call rank0_alb(c)

  ! kind=1, fixed length
  call rank0_fl (null(c))
  call rank0_fl (null())
  call rank0_fl (c)
  call arank_fl (null(c))
  call arank_fl (c)
  call rank0_flb(null(c))
  call rank0_flb(c)

  ! kind=1, deferred length
  call rank0_dl (null(d))
  call rank0_dl (null())
  call rank0_dl (d)
  call arank_dl (null(d))   ! <- this crashes nagfor 7.2
  call arank_dl (d)
  call rank0_dlb(null(d))
  call rank0_dlb(null())

  ! rank-1 character variables:
  ! kind=1, assumed length
  call rank1_al (null(c1))
  call rank1_al (c1)
  call arank_al (null(c1))
  call arank_al (c1)
  call rank1_alb(null(c1))
  call rank1_alb(c1)

  ! kind=1, fixed length
  call rank1_fl (null(c1))
  call rank1_fl (null())
  call rank1_fl (c1)
  call arank_fl (null(c1))
  call arank_fl (c1)
  call rank1_flb(null(c1))
  call rank1_flb(c1)

  ! kind=1, deferred length
  call rank1_dl (null(d1))
  call rank1_dl (null())
  call rank1_dl (d1)
  call arank_dl (null(d1))
  call arank_dl (d1)
  call rank1_dlb(null(d1))
  call rank1_dlb(null())

  ! kind=4, assumed length
  call rank1_al_4 (null(c4))
  call rank1_al_4 (c4)
  call arank_al_4 (null(c4))
  call arank_al_4 (c4)
  call rank1_al_4b(null(c4))
  call rank1_al_4b(c4)

  ! kind=4, fixed length
  call rank1_fl_4 (null(c4))
  call rank1_fl_4 (null())
  call rank1_fl_4 (c4)
  call arank_fl_4 (null(c4))
  call arank_fl_4 (c4)
  call rank1_fl_4b(null(c4))
  call rank1_fl_4b(c4)

  ! kind=4, deferred length
  call rank1_dl_4 (null(d4))
  call rank1_dl_4 (null())
  call rank1_dl_4 (d4)
  call arank_dl_4 (null(d4))
  call arank_dl_4 (d4)
  call rank1_dl_4b(null(d4))
  call rank1_dl_4b(null())

contains

  ! kind=4, rank=1
  subroutine rank1_al_4 (x)
    character(kind=4,len=*),  pointer, intent(in) :: x(:)
    if (associated (x)) stop 41
  end
  subroutine rank1_fl_4 (x)
    character(kind=4,len=10), pointer, intent(in) :: x(:)
    if (associated (x)) stop 42
  end
  subroutine rank1_dl_4 (x)
    character(kind=4,len=:),  pointer, intent(in) :: x(:)
    if (associated (x)) stop 43
  end

  subroutine rank1_al_4b (y)
    character(kind=4,len=*),  pointer, intent(in) :: y(:)
    call rank1_al_4 (y)
    if (associated (y)) stop 44
  end
  subroutine rank1_fl_4b (y)
    character(kind=4,len=10), pointer, intent(in) :: y(:)
    call rank1_fl_4 (y)
    if (associated (y)) stop 45
  end
  subroutine rank1_dl_4b (y)
    character(kind=4,len=:),  pointer, intent(in) :: y(:)
    call rank1_dl_4 (y)
    if (associated (y)) stop 46
  end

  ! kind=4, assumed-rank versions
  subroutine arank_al_4 (x)
    character(kind=4,len=*),  pointer, intent(in) :: x(..)
    if (associated (x)) stop 47
    ! this testcase assumes that we call this subroutine only with rank=1
    if (rank (x) /= 1)  stop 57
  end
  subroutine arank_fl_4 (x)
    character(kind=4,len=10), pointer, intent(in) :: x(..)
    if (associated (x)) stop 48
    ! this testcase assumes that we call this subroutine only with rank=1
    if (rank (x) /= 1)  stop 58
  end
  subroutine arank_dl_4 (x)
    character(kind=4,len=:), pointer, intent(in) :: x(..)
    if (associated (x)) stop 49
    ! this testcase assumes that we call this subroutine only with rank=1
    if (rank (x) /= 1)  stop 59
  end

  ! kind=1, rank=1
  subroutine rank1_al (x)
    character(kind=1,len=*),  pointer, intent(in) :: x(:)
    if (associated (x)) stop 11
  end
  subroutine rank1_fl (x)
    character(kind=1,len=10), pointer, intent(in) :: x(:)
    if (associated (x)) stop 12
  end
  subroutine rank1_dl (x)
    character(kind=1,len=:),  pointer, intent(in) :: x(:)
    if (associated (x)) stop 13
  end

  subroutine rank1_alb (y)
    character(kind=1,len=*),  pointer, intent(in) :: y(:)
    call rank1_al (y)
    if (associated (y)) stop 14
  end
  subroutine rank1_flb (y)
    character(kind=1,len=10), pointer, intent(in) :: y(:)
    call rank1_fl (y)
    if (associated (y)) stop 15
  end
  subroutine rank1_dlb (y)
    character(kind=1,len=:),  pointer, intent(in) :: y(:)
    call rank1_dl (y)
    if (associated (y)) stop 16
  end

  ! kind=1, assumed-rank versions
  subroutine arank_al (x)
    character(kind=1,len=*),  pointer, intent(in) :: x(..)
    if (associated (x)) stop 17
  end
  subroutine arank_fl (x)
    character(kind=1,len=10), pointer, intent(in) :: x(..)
    if (associated (x)) stop 18
  end
  subroutine arank_dl (x)
    character(kind=1,len=:), pointer, intent(in) :: x(..)
    if (associated (x)) stop 19
  end

  ! kind=1, scalar
  subroutine rank0_al (x)
    character(kind=1,len=*),  pointer, intent(in) :: x
    if (associated (x)) stop  1
  end
  subroutine rank0_fl (x)
    character(kind=1,len=10), pointer, intent(in) :: x
    if (associated (x)) stop  2
  end
  subroutine rank0_dl (x)
    character(kind=1,len=:),  pointer, intent(in) :: x
    if (associated (x)) stop  3
  end

  subroutine rank0_alb (y)
    character(kind=1,len=*),  pointer, intent(in) :: y
    call rank0_al (y)
    if (associated (y)) stop  4
  end
  subroutine rank0_flb (y)
    character(kind=1,len=10), pointer, intent(in) :: y
    call rank0_fl (y)
    if (associated (y)) stop  5
  end
  subroutine rank0_dlb (y)
    character(kind=1,len=:),  pointer, intent(in) :: y
    call rank0_dl (y)
    if (associated (y)) stop  6
  end

end
