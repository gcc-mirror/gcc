! { dg-lto-do link }
! { dg-lto-options {{-g -flto -Wno-lto-type-mismatch} {-g -O -flto -Wno-lto-type-mismatch}} }
program species
integer spk(2)
real eval(2)
interface
  subroutine atom(sol,k,eval)
    real, intent(in) :: sol
    integer, intent(in) :: k(2)
    real, intent(out) :: eval(2)
  end subroutine
end interface
spk = 2
call atom(1.1,spk,eval)
end program

