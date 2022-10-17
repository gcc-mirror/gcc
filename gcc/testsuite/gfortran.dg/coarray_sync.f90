! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/99351 - ICE in gfc_finish_var_decl, at fortran/trans-decl.c:695

module m
  character(3), parameter   :: c = 'abc'
  integer,      parameter   :: s = 42
  integer,      target      :: i
  character(:), allocatable :: a
  target :: a
contains
  subroutine s1
    allocate (character(42) :: a)
    sync all (stat=i)
    sync all (stat=f())
    sync all (errmsg=a)
    sync all (errmsg=p())
    sync all (stat=a%len) ! { dg-error "variable definition context" }
    sync all (stat=s)     ! { dg-error "variable definition context" }
    sync all (errmsg=c)   ! { dg-error "variable definition context" }
  end
  subroutine s2
    sync images (*, stat=i)
    sync images (*, errmsg=a)
    sync images (*, stat=a%len) ! { dg-error "variable definition context" }
    sync images (*, stat=s)     ! { dg-error "variable definition context" }
    sync images (*, errmsg=c)   ! { dg-error "variable definition context" }
  end
  subroutine s3
    sync memory (stat=i,errmsg=p())
    sync memory (stat=f(),errmsg=a)
    sync memory (stat=a%len) ! { dg-error "variable definition context" }
    sync memory (stat=s)     ! { dg-error "variable definition context" }
    sync memory (errmsg=c)   ! { dg-error "variable definition context" }
  end
  integer function f()
    pointer :: f
    f => i
  end function f
  function p()
    character(:), pointer :: p
    p => a
  end function p
end
