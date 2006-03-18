! { dg-do run }
! pr16935
! segfault at run time on open statement
       program bug2
       implicit none
       open( 1 , file = "str_500.txt", position = "REWIND" )
       close( 1 , status = "DELETE" )
       end
