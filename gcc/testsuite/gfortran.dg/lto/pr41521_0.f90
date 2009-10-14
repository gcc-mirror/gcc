! { dg-lto-do link }
! { dg-lto-options {{-g -flto} {-g -O -flto}} }
program species
integer spk(2)
real eval(2)
spk = 2
call atom(1.1,spk,eval)
end program

