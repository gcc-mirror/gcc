! PR 14038- 'H' in hollerith causes mangling of string
program hollerith
  IMPLICIT NONE
  CHARACTER*4 LINE
100 FORMAT (4H12H4)
  WRITE(LINE,100)
  IF (LINE .NE. '12H4') call abort ()
end

