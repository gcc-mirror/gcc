! PR fortran/92568
!
implicit none
  type t
  end type t

  integer :: ii
  integer :: arr(5)
  integer, allocatable :: aii, aarr(:)
  integer, pointer :: pii, parr(:)

  character :: str1, str1arr(5), str1a, str1aarr(:), str1p, str1parr(:)
  character(len=5) :: str5, str5arr(5), str5a, str5aarr(:), str5p, str5parr(:)
  character(len=:) :: strXa, strXaarr(:), strXp, strXparr(:)
  allocatable :: str1a, str1aarr, str5a, str5aarr, strXa, strXaarr
  pointer :: str1p, str1parr, str5p, str5parr, strXp, strXparr

  type(t) :: dt, dtarr(5), dta, dtaarr(:), dtp, dtparr(:)
  allocatable :: dta, dtaarr
  pointer :: dtp, dtparr

  allocate(aii, aarr(5), str1a, str1aarr(5), dta, dtparr(5))
  allocate(pii, parr(5), str1p, str1parr(5), dtp, dtparr(5))
  allocate(character(len=7) :: strXa, strXaarr(5), strXp, strXparr(5))


  !$omp target defaultmap ( none )  ! { dg-note "enclosing 'target'" }
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    ! { dg-error "'ii' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'aii' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'pii' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    ! { dg-error "'str1' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'str1arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'str1a' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'str1aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'str1p' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'str1parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    ! { dg-error "'str5' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'str5arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'str5a' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'str5aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'str5p' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'str5parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    ! { dg-error "'strxa' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'strxaarr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'strxp' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'strxparr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }

    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
    ! { dg-error "'dt' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'dtarr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'dta' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'dtaarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'dtp' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'dtparr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }
  !$omp end target


  !$omp target defaultmap(none : scalar)  defaultmap(none : aggregate)  &
  !$omp&       defaultmap(none : allocatable) defaultmap(none : pointer)   ! { dg-note "enclosing 'target'" }
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    ! { dg-error "'ii' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'aii' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'pii' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    ! { dg-error "'str1' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'str1arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'str1a' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'str1aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'str1p' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'str1parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    ! { dg-error "'str5' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'str5arr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'str5a' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'str5aarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'str5p' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'str5parr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }

    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    ! { dg-error "'strxa' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'strxaarr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'strxp' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'strxparr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }

    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
    ! { dg-error "'dt' not specified in enclosing 'target'" "" { target *-*-* } .-1 }
    ! { dg-error "'dtarr' not specified in enclosing 'target'" "" { target *-*-* } .-2 }
    ! { dg-error "'dta' not specified in enclosing 'target'" "" { target *-*-* } .-3 }
    ! { dg-error "'dtaarr' not specified in enclosing 'target'" "" { target *-*-* } .-4 }
    ! { dg-error "'dtp' not specified in enclosing 'target'" "" { target *-*-* } .-5 }
    ! { dg-error "'dtparr' not specified in enclosing 'target'" "" { target *-*-* } .-6 }
  !$omp end target
end
