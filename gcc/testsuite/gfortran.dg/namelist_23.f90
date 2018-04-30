!{ dg-do run { target fd_truncate } }
! PR26136 Filling logical variables from namelist read when object list is not
! complete.  Test case derived from PR.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program read_logical
   implicit none
   logical, dimension(4) :: truely
   integer, dimension(4) :: truely_a_very_long_variable_name
   namelist /mynml/ truely
   namelist /mynml/ truely_a_very_long_variable_name

   truely = .false.
   truely_a_very_long_variable_name = 0

   open(10, status="scratch")
   write(10,*) "&mynml"
   write(10,*) "truely       = trouble,    traffic .true"
   write(10,*) "truely_a_very_long_variable_name  = 4,      4,      4"
   write(10,*) "/"
   rewind(10)
   read (10, nml=mynml, err = 1000)
   if (.not.all(truely(1:3))) STOP 1
   if (.not.all(truely_a_very_long_variable_name(1:3).eq.4)) STOP 2
   
   truely = .false.
   truely_a_very_long_variable_name = 0

   rewind(10)
   write(10,*) "&mynml"
   write(10,*) "truely       = .true., .true.,"
   write(10,*) "truely_a_very_long_variable_name  = 4,      4,      4"
   write(10,*) "/"
   rewind(10)
   read (10, nml=mynml, err = 1000)
   if (.not.all(truely(1:2))) STOP 3
   if (.not.all(truely_a_very_long_variable_name(1:3).eq.4)) STOP 4

   truely = .true.
   truely_a_very_long_variable_name = 0

   rewind(10)
   write(10,*) "&mynml"
   write(10,*) "truely       = .false., .false.,"
   write(10,*) "truely_a_very_long_variable_name  = 4,      4,      4"
   write(10,*) "/"
   rewind(10)
   read (10, nml=mynml, err = 1000)
   if (all(truely(1:2))) STOP 5
   if (.not.all(truely_a_very_long_variable_name(1:3).eq.4)) STOP 6
   close(10)
   stop
1000 STOP 7
end program read_logical
