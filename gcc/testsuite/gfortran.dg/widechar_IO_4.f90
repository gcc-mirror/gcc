! { dg-do run }
! { dg-options -fbackslash }
! Wide chracter I/O test 4, formatted ISO-8859-1 characters in string
! Test case developed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
! Compile with -fbackslash
integer, parameter  :: k4 = 4
character(kind=1,len=15) :: buffer
character(kind=1, len=1) :: c1, c2
character(kind=4,len=20) :: str = k4_'X\xF8öABC' ! ISO-8859-1 encoded string
buffer = ""
write(buffer,'(3a)')':',trim(str),':'
if (buffer.ne.':X\xF8öABC: ') call abort
str = ""
read(buffer,'(3a)') c1,str(1:6),c2
if (c1.ne.':') call abort
if (str.ne.k4_'X\xF8öAB') call abort
if (c2.ne.'C') call abort
end
