! { dg-do compile }

subroutine f1
  type dt
    logical :: l = .false.
  end type
  type dt2
    logical :: l = .false.
  end type
!$omp declare reduction (foo:integer(kind = 4) & ! { dg-error "Previous !.OMP DECLARE REDUCTION" }
!$omp & :omp_out = omp_out + omp_in)
!$omp declare reduction (foo:integer(kind = 4) : & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION" }
!$omp & omp_out = omp_out + omp_in)
!$omp declare reduction (bar:integer, &
!$omp & real:omp_out = omp_out + omp_in)
!$omp declare reduction (baz:integer,real,integer & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION|Previous" }
!$omp & : omp_out = omp_out + omp_in)
!$omp declare reduction (id1:dt,dt2:omp_out%l=omp_out%l &
!$omp & .or.omp_in%l)
!$omp declare reduction (id2:dt,dt:omp_out%l=omp_out%l & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION|Previous" }
!$omp & .or.omp_in%l)
!$omp declare reduction (id3:dt2,dt:omp_out%l=omp_out%l & ! { dg-error "Previous !.OMP DECLARE REDUCTION" }
!$omp & .or.omp_in%l)
!$omp declare reduction (id3:dt2:omp_out%l=omp_out%l & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION" }
!$omp & .or.omp_in%l)
end subroutine f1
subroutine f2
  interface
    subroutine f2a (x, y, z)
      character (len = *) :: x, y
      logical :: z
    end subroutine
  end interface
  interface f2b
    subroutine f2b (x, y, z)
      character (len = *, kind = 1) :: x, y
      logical :: z
    end subroutine
    subroutine f2c (x, y, z)
      character (kind = 4, len = *) :: x, y
      logical :: z
    end subroutine
  end interface
!$omp declare reduction (foo:character(len=*): &
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (bar:character(len=:): &
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (baz:character(len=4): &
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (baz:character(len=5): &
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (baz:character(len=6): &
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (id:character(len=*): & ! { dg-error "Previous !.OMP DECLARE REDUCTION" }
!$omp & f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (id: & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION" }
!$omp & character(len=:) : f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction & ! { dg-error "Redefinition of !.OMP DECLARE REDUCTION|Previous" }
!$omp (id2:character(len=*), character(len=:): &
!$omp f2a (omp_out, omp_in, .false.)) &
!$omp & initializer (f2a (omp_priv, omp_orig, .true.))
!$omp declare reduction (id3:character(len=*, kind = 1), character(kind=4, len=:): &
!$omp f2b (omp_out, omp_in, .false.)) &
!$omp & initializer (f2b (omp_priv, omp_orig, .true.))
!$omp declare reduction (id4:character(kind=4, len=4), character(kind =1, len=4): &
!$omp f2b (omp_out, omp_in, .false.)) &
!$omp & initializer (f2b (omp_priv, omp_orig, .true.))
end subroutine f2
