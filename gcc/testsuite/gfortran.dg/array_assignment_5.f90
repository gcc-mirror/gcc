! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 62214 - this used to give the wrong result.
! Original test case by Oliver Fuhrer
PROGRAM test
  IMPLICIT NONE
  CHARACTER(LEN=20)   :: fullNames(2)
  CHARACTER(LEN=255)  :: pathName
  CHARACTER(LEN=5)    :: fileNames(2)
  
  pathName = "/dir1/dir2/"
  fileNames = (/ "file1", "file2" /)
  fullNames = SPREAD(TRIM(pathName),1,2) // fileNames
  if (fullNames(1) /= '/dir1/dir2/file1' .or. &
       & fullnames(2) /= '/dir1/dir2/file2') STOP 1
END PROGRAM test
