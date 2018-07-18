! { dg-do link }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program coarray_43
  implicit none
  integer, parameter :: STR_LEN = 50
  character(len=STR_LEN) :: str[*]
  integer :: pos
  write(str,"(2(a,i2))") "Greetings from image ",this_image()," of ",num_images()
  block
    pos = scan(str[5], set="123456789")
  end block
end program
