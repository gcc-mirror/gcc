! PR fortran/21647
program inquire_5
  integer (kind = 8) :: unit8
  logical (kind = 8) :: exist8
  integer (kind = 4) :: unit4
  logical (kind = 4) :: exist4
  integer (kind = 2) :: unit2
  logical (kind = 2) :: exist2
  integer (kind = 1) :: unit1
  logical (kind = 1) :: exist1
  character (len = 6) :: del
  unit8 = 78
  open (file = 'inquire_5.txt', unit = unit8)
  unit8 = -1
  exist8 = .false.
  unit4 = -1
  exist4 = .false.
  unit2 = -1
  exist2 = .false.
  unit1 = -1
  exist1 = .false.
  inquire (file = 'inquire_5.txt', number = unit8, exist = exist8)
  if (unit8 .ne. 78 .or. .not. exist8) call abort
  inquire (file = 'inquire_5.txt', number = unit4, exist = exist4)
  if (unit4 .ne. 78 .or. .not. exist4) call abort
  inquire (file = 'inquire_5.txt', number = unit2, exist = exist2)
  if (unit2 .ne. 78 .or. .not. exist2) call abort
  inquire (file = 'inquire_5.txt', number = unit1, exist = exist1)
  if (unit1 .ne. 78 .or. .not. exist1) call abort
  del = 'delete'
  close (unit = 78, status = del)
end
