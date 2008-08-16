! { dg-do run }
! { dg-options "-fbackslash" }
! Contributed by Tobias Burnus
program test2
  integer,parameter :: ucs4 = selected_char_kind("iso_10646")
  character(1,ucs4),parameter :: nen=char(int(z'5e74'),ucs4), & !year
    gatsu=char(int(z'6708'),kind=ucs4), & !month
    nichi=char(int(z'65e5'),kind=ucs4) !day
  character(25,ucs4) :: string
  open(10, encoding="utf-8", status="scratch")
  write(10,1) 2008,nen,8,gatsu,10,nichi
1 format(i0,a,i0,a,i0,a)
  rewind(10)
  read(10,'(a)') string
  if (string /= ucs4_"2008\u5e748\u670810\u65e5") call abort
end program test2
