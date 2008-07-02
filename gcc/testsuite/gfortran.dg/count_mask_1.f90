! { dg-do run }
! PR 36590, PR 36681
program test
  logical(kind=1),parameter     :: t=.true.,f=.false.
  logical(kind=1),dimension(9)  :: hexa,hexb
  data hexa/f,f,t,t,f,f,f,t,f/,hexb/f,t,f,f,f,t,t,f,f/
  isum=count(hexa(1:9).eqv.hexb(1:9))
end program   
