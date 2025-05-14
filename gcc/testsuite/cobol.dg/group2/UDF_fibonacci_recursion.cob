       *> { dg-do run }
       *> { dg-output-file "group2/UDF_fibonacci_recursion.out" }

        identification division.
        function-id. fib-func.
        data division.
        working-storage section.
        01 instance pic 9999 value 0.
        linkage section.
        01 n binary-char unsigned.
        01 f-n binary-long unsigned.
        procedure division using n returning f-n.
           evaluate true
           when n = 0
                move 0 to f-n
           when n = 1
                move 1 to f-n
           when other
                compute f-n =  fib-func(n - 1) + fib-func(n - 2)
           end-evaluate
           goback .
        end function fib-func.

        identification division.
        program-id. pmain.
        environment division.
        configuration section.
        repository.
           function fib-func.
        data division.
        working-storage section.
        01 n binary-char unsigned.
        procedure division.
           perform varying n from 1 by 1 until n > 16
                display "fibonacci(" n "): " fib-func(n)
           end-perform
           stop run.
        end program pmain.

