C     Substring range checking test program, to check behavior with respect
C     to X3J3/90.4 paragraph 5.7.1.
C
C     Patches relax substring checking for subscript expressions in order to
C     simplify coding (elimination of length checks for strings passed as
C     parameters) and to avoid contradictory behavior of subscripted substring
C     expressions with respect to unsubscripted string expressions.
C
C     Key part of 5.7.1 interpretation comes down to statement that in the
C     substring expression,
C        v ( e1 : e2 )
C     1 <= e1 <= e2 <= len to be valid, yet the expression
C        v ( : )
C     is equivalent to
C        v(1:len(v))
C
C     meaning that any statement that reads
C        str = v // 'tail'
C     (where v is a string passed as a parameter) would require coding as
C        if (len(v) .gt. 0) then
C           str = v // 'tail'
C        else
C           str = 'tail'
C        endif
C     to comply with the standard specification.  Under the stricter
C     interpretation, functions strcat and strlat would be incorrect as
C     written for null values of str1 and/or str2.
C
C     This code compiles and runs without error on
C       SunOS 4.1.3 f77 (-C option)
C       SUNWspro SPARCcompiler 4.2 f77 (-C option)
C       (and with proposed patches, gcc-2.9.2 -fbounds-check except for test 6,
C        which is a genuine, deliberate error - comment out to make further
C        tests)
C
C { dg-do run }
C { dg-options "-fbounds-check" }
C
C     G. Helffrich/Tokyo Inst. Technology Jul 24 2001

      character str*8,strres*16,strfun*16,strcat*16,strlat*16

      str='Hi there'

C     Test 1 - (current+patched) two char substring result
      strres=strfun(str,1,2)
      write(*,*) 'strres is ',strres

C     Test 2 - (current+patched) null string result
      strres=strfun(str,5,4)
      write(*,*) 'strres is ',strres

C     Test 3 - (current+patched) null string result
      strres=strfun(str,8,7)
      write(*,*) 'strres is ',strres

C     Test 4 - (current) error; (patched) null string result
      strres=strfun(str,9,8)
      write(*,*) 'strres is ',strres

C     Test 5 - (current) error; (patched) null string result
      strres=strfun(str,1,0)
      write(*,*) 'strres is ',strres

C     Test 6 - (current+patched) error
C     strres=strfun(str,20,20)
C     write(*,*) 'strres is ',strres

C     Test 7 - (current+patched) str result
      strres=strcat(str,'')
      write(*,*) 'strres is ',strres

C     Test 8 - (current) error; (patched) str result
      strres=strlat('',str)
      write(*,*) 'strres is ',strres

      end

      character*(*) function strfun(str,i,j)
      character str*(*)

      strfun = str(i:j)
      end

      character*(*) function strcat(str1,str2)
      character str1*(*), str2*(*)

      strcat = str1 // str2
      end

      character*(*) function strlat(str1,str2)
      character str1*(*), str2*(*)

      strlat = str1(1:len(str1)) // str2(1:len(str2))
      end
