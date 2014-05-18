! { dg-do run { xfail powerpc*-apple-darwin* } }
! PR52539 UTF-8 support for namelist read and write

character(len=10, kind=4) :: str, str2
character(len=25, kind=4) :: str3

namelist /nml/ str

str = 4_'1a'//char (int (z'4F60'),4)     &
      //char (int (z'597D'), 4)//4_'b'
open(6, encoding='utf-8')
open(99, encoding='utf-8',form='formatted')
write(99, '(3a)') '&nml str = "', str, '" /'
write(99, '(a)') str
rewind(99)

str = 4_'XXXX'
str2 = 4_'YYYY'
read(99,nml=nml)
read(99, *) str2
if (str2 /= str) call abort
rewind(99)

read(99,'(A)') str3
if (str3 /= 4_'&nml str = "' // str // 4_'" /') call abort
read(99,*) str3
if (str3 /= str) call abort

close(99, status='delete')
end
