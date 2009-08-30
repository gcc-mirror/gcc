! { dg-do compile }
SUBROUTINE write_cputime( checkpoint  )
  CHARACTER(LEN=*), INTENT(IN)             :: checkpoint
  CHARACTER(LEN=LEN_TRIM(checkpoint)+7)    :: string1
    string1 = ADJUSTL(string1)
END SUBROUTINE write_cputime
