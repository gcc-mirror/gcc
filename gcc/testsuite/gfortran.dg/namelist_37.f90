! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR33039 Read NAMELIST:  reads wrong namelist name
! Test case from PR modified by Jerry DeLisle <jvdelisle@gcc.gnu.org>
PROGRAM namelist
CHARACTER*25 CHAR
NAMELIST /CODE/ CHAR, X
NAMELIST /CODEtwo/ X

OPEN(10, status="scratch")
write(10,'(a)') "File with test NAMELIST inputs"
write(10,'(a)') " &CODVJS  char='VJS-Not a proper nml name', X=-0.5/"
write(10,'(a)') " &CODEone char='CODEone input', X=-1.0 /"
write(10,'(a)') " &CODEtwo char='CODEtwo inputs', X=-2.0/"
write(10,'(a)') " &code    char='Lower case name',X=-3.0/"
write(10,'(a)') " &CODE    char='Desired namelist sel', X=44./"
write(10,'(a)') " &CODEx   char='Should not read CODEx nml', X=-5./"
write(10,'(a)') " $CODE    char='Second desired nml', X=66.0 /"
write(10,'(a)') " $CODE    X=77.0, char='Reordered desired nml'/"
rewind(10)
CHAR = 'Initialize string ***'
X    = -777.
READ(10, nml=CODE, END=999)
if (x.ne.-3.0) call abort
READ(10, nml=CODE, END=999)
if (x.ne.44.0) call abort
READ(10, nml=CODE, END=999)
if (x.ne.66.0) call abort
READ(10, nml=CODE, END=999)
 999 if (x.ne.77.0) call abort
END PROGRAM namelist
