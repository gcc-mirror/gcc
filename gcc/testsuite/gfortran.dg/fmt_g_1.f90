! { dg-do run }
! PR59771 Cleanup handling of Gw.0 and Gw.0Ee format
! Test case prepared by Dominique d'Humieres <dominiq@lps.ens.fr>
       PROGRAM FOO
       character(len=60) :: buffer, buffer1

       write (buffer ,'(6(1X,1PG9.0e2))') 0.0, 0.04, 0.06, 0.4, 0.6, 243.0
       write (buffer1,'(6(1X,1PE9.0e2))') 0.0, 0.04, 0.06, 0.4, 0.6, 243.0

       if (buffer /= buffer1) call abort
       end
