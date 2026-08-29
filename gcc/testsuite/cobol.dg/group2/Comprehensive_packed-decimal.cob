      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/Comprehensive_packed-decimal.out" }
       identification division.
       program-id. packed-decimal-move-test.
       data division.
       working-storage section.
       01 var11       pic    9  packed-decimal value     4.
       01 var12       pic   99  packed-decimal value    34.
       01 var13       pic  999  packed-decimal value   234.
       01 var14       pic 9999  packed-decimal value  1234.
       01 var21       pic    s9 packed-decimal value     4.
       01 var22       pic   s99 packed-decimal value    34.
       01 var23       pic  s999 packed-decimal value   234.
       01 var24       pic s9999 packed-decimal value  1234.
       01 var21n      pic    s9 packed-decimal value    -4.
       01 var22n      pic   s99 packed-decimal value   -34.
       01 var23n      pic  s999 packed-decimal value  -234.
       01 var24n      pic s9999 packed-decimal value -1234.
       01 var31       pic    9  packed-decimal with no sign
                                                       value    4.
       01 var32       pic   99  packed-decimal with no sign
                                                       value   34.
       01 var33       pic  999  packed-decimal with no sign
                                                       value  234.
       01 var34       pic 9999  packed-decimal with no sign
                                                       value 1234.
       01 var11d      pic    9  packed-decimal.
       01 var12d      pic   99  packed-decimal.
       01 var13d      pic  999  packed-decimal.
       01 var14d      pic 9999  packed-decimal.
       01 var21d      pic    s9 packed-decimal.
       01 var22d      pic   s99 packed-decimal.
       01 var23d      pic  s999 packed-decimal.
       01 var24d      pic s9999 packed-decimal.
       01 var21nd     pic    s9 packed-decimal.
       01 var22nd     pic   s99 packed-decimal.
       01 var23nd     pic  s999 packed-decimal.
       01 var24nd     pic s9999 packed-decimal.
       01 var31d      pic    9  packed-decimal with no sign.
       01 var32d      pic   99  packed-decimal with no sign.
       01 var33d      pic  999  packed-decimal with no sign.
       01 var34d      pic 9999  packed-decimal with no sign.
       01 failure-count pic 9(3) comp-5 value 0.
       procedure division.
       test-packed-decimal.
           move var11 to var11d
           if var11d <> 4
               display "test1 no good " var11d
               add 1 to failure-count
           end-if
           move var11 to var12d
           if var12d <> 4
               display "test2 no good " var12d
               add 1 to failure-count
           end-if
           move var11 to var13d
           if var13d <> 4
               display "test3 no good " var13d
               add 1 to failure-count
           end-if
           move var11 to var14d
           if var14d <> 4
               display "test4 no good " var14d
               add 1 to failure-count
           end-if
           move var12 to var11d
           if var11d <> 4
               display "test5 no good " var11d
               add 1 to failure-count
           end-if
           move var12 to var12d
           if var12d <> 34
               display "test6 no good " var12d
               add 1 to failure-count
           end-if
           move var12 to var13d
           if var13d <> 34
               display "test7 no good " var13d
               add 1 to failure-count
           end-if
           move var12 to var14d
           if var14d <> 34
               display "test8 no good " var14d
               add 1 to failure-count
           end-if
           move var13 to var11d
           if var11d <> 4
               display "test9 no good " var11d
               add 1 to failure-count
           end-if
           move var13 to var12d
           if var12d <> 34
               display "test10 no good " var12d
               add 1 to failure-count
           end-if
           move var13 to var13d
           if var13d <> 234
               display "test11 no good " var13d
               add 1 to failure-count
           end-if
           move var13 to var14d
           if var14d <> 234
               display "test12 no good " var14d
               add 1 to failure-count
           end-if
           move var14 to var11d
           if var11d <> 4
               display "test13 no good " var11d
               add 1 to failure-count
           end-if
           move var14 to var12d
           if var12d <> 34
               display "test14 no good " var12d
               add 1 to failure-count
           end-if
           move var14 to var13d
           if var13d <> 234
               display "test15 no good " var13d
               add 1 to failure-count
           end-if
           move var14 to var14d
           if var14d <> 1234
               display "test16 no good " var14d
               add 1 to failure-count
           end-if
           move var11 to var21d
           if var21d <> 4
               display "test17 no good " var21d
               add 1 to failure-count
           end-if
           move var11 to var22d
           if var22d <> 4
               display "test18 no good " var22d
               add 1 to failure-count
           end-if
           move var11 to var23d
           if var23d <> 4
               display "test19 no good " var23d
               add 1 to failure-count
           end-if
           move var11 to var24d
           if var24d <> 4
               display "test20 no good " var24d
               add 1 to failure-count
           end-if
           move var12 to var21d
           if var21d <> 4
               display "test21 no good " var21d
               add 1 to failure-count
           end-if
           move var12 to var22d
           if var22d <> 34
               display "test22 no good " var22d
               add 1 to failure-count
           end-if
           move var12 to var23d
           if var23d <> 34
               display "test23 no good " var23d
               add 1 to failure-count
           end-if
           move var12 to var24d
           if var24d <> 34
               display "test24 no good " var24d
               add 1 to failure-count
           end-if
           move var13 to var21d
           if var21d <> 4
               display "test25 no good " var21d
               add 1 to failure-count
           end-if
           move var13 to var22d
           if var22d <> 34
               display "test26 no good " var22d
               add 1 to failure-count
           end-if
           move var13 to var23d
           if var23d <> 234
               display "test27 no good " var23d
               add 1 to failure-count
           end-if
           move var13 to var24d
           if var24d <> 234
               display "test28 no good " var24d
               add 1 to failure-count
           end-if
           move var14 to var21d
           if var21d <> 4
               display "test29 no good " var21d
               add 1 to failure-count
           end-if
           move var14 to var22d
           if var22d <> 34
               display "test30 no good " var22d
               add 1 to failure-count
           end-if
           move var14 to var23d
           if var23d <> 234
               display "test31 no good " var23d
               add 1 to failure-count
           end-if
           move var14 to var24d
           if var24d <> 1234
               display "test32 no good " var24d
               add 1 to failure-count
           end-if
           move var11 to var21nd
           if var21nd <> 4
               display "test33 no good " var21nd
               add 1 to failure-count
           end-if
           move var11 to var22nd
           if var22nd <> 4
               display "test34 no good " var22nd
               add 1 to failure-count
           end-if
           move var11 to var23nd
           if var23nd <> 4
               display "test35 no good " var23nd
               add 1 to failure-count
           end-if
           move var11 to var24nd
           if var24nd <> 4
               display "test36 no good " var24nd
               add 1 to failure-count
           end-if
           move var12 to var21nd
           if var21nd <> 4
               display "test37 no good " var21nd
               add 1 to failure-count
           end-if
           move var12 to var22nd
           if var22nd <> 34
               display "test38 no good " var22nd
               add 1 to failure-count
           end-if
           move var12 to var23nd
           if var23nd <> 34
               display "test39 no good " var23nd
               add 1 to failure-count
           end-if
           move var12 to var24nd
           if var24nd <> 34
               display "test40 no good " var24nd
               add 1 to failure-count
           end-if
           move var13 to var21nd
           if var21nd <> 4
               display "test41 no good " var21nd
               add 1 to failure-count
           end-if
           move var13 to var22nd
           if var22nd <> 34
               display "test42 no good " var22nd
               add 1 to failure-count
           end-if
           move var13 to var23nd
           if var23nd <> 234
               display "test43 no good " var23nd
               add 1 to failure-count
           end-if
           move var13 to var24nd
           if var24nd <> 234
               display "test44 no good " var24nd
               add 1 to failure-count
           end-if
           move var14 to var21nd
           if var21nd <> 4
               display "test45 no good " var21nd
               add 1 to failure-count
           end-if
           move var14 to var22nd
           if var22nd <> 34
               display "test46 no good " var22nd
               add 1 to failure-count
           end-if
           move var14 to var23nd
           if var23nd <> 234
               display "test47 no good " var23nd
               add 1 to failure-count
           end-if
           move var14 to var24nd
           if var24nd <> 1234
               display "test48 no good " var24nd
               add 1 to failure-count
           end-if
           move var11 to var31d
           if var31d <> 4
               display "test49 no good " var31d
               add 1 to failure-count
           end-if
           move var11 to var32d
           if var32d <> 4
               display "test50 no good " var32d
               add 1 to failure-count
           end-if
           move var11 to var33d
           if var33d <> 4
               display "test51 no good " var33d
               add 1 to failure-count
           end-if
           move var11 to var34d
           if var34d <> 4
               display "test52 no good " var34d
               add 1 to failure-count
           end-if
           move var12 to var31d
           if var31d <> 4
               display "test53 no good " var31d
               add 1 to failure-count
           end-if
           move var12 to var32d
           if var32d <> 34
               display "test54 no good " var32d
               add 1 to failure-count
           end-if
           move var12 to var33d
           if var33d <> 34
               display "test55 no good " var33d
               add 1 to failure-count
           end-if
           move var12 to var34d
           if var34d <> 34
               display "test56 no good " var34d
               add 1 to failure-count
           end-if
           move var13 to var31d
           if var31d <> 4
               display "test57 no good " var31d
               add 1 to failure-count
           end-if
           move var13 to var32d
           if var32d <> 34
               display "test58 no good " var32d
               add 1 to failure-count
           end-if
           move var13 to var33d
           if var33d <> 234
               display "test59 no good " var33d
               add 1 to failure-count
           end-if
           move var13 to var34d
           if var34d <> 234
               display "test60 no good " var34d
               add 1 to failure-count
           end-if
           move var14 to var31d
           if var31d <> 4
               display "test61 no good " var31d
               add 1 to failure-count
           end-if
           move var14 to var32d
           if var32d <> 34
               display "test62 no good " var32d
               add 1 to failure-count
           end-if
           move var14 to var33d
           if var33d <> 234
               display "test63 no good " var33d
               add 1 to failure-count
           end-if
           move var14 to var34d
           if var34d <> 1234
               display "test64 no good " var34d
               add 1 to failure-count
           end-if
           move var21 to var11d
           if var11d <> 4
               display "test65 no good " var11d
               add 1 to failure-count
           end-if
           move var21 to var12d
           if var12d <> 4
               display "test66 no good " var12d
               add 1 to failure-count
           end-if
           move var21 to var13d
           if var13d <> 4
               display "test67 no good " var13d
               add 1 to failure-count
           end-if
           move var21 to var14d
           if var14d <> 4
               display "test68 no good " var14d
               add 1 to failure-count
           end-if
           move var22 to var11d
           if var11d <> 4
               display "test69 no good " var11d
               add 1 to failure-count
           end-if
           move var22 to var12d
           if var12d <> 34
               display "test70 no good " var12d
               add 1 to failure-count
           end-if
           move var22 to var13d
           if var13d <> 34
               display "test71 no good " var13d
               add 1 to failure-count
           end-if
           move var22 to var14d
           if var14d <> 34
               display "test72 no good " var14d
               add 1 to failure-count
           end-if
           move var23 to var11d
           if var11d <> 4
               display "test73 no good " var11d
               add 1 to failure-count
           end-if
           move var23 to var12d
           if var12d <> 34
               display "test74 no good " var12d
               add 1 to failure-count
           end-if
           move var23 to var13d
           if var13d <> 234
               display "test75 no good " var13d
               add 1 to failure-count
           end-if
           move var23 to var14d
           if var14d <> 234
               display "test76 no good " var14d
               add 1 to failure-count
           end-if
           move var24 to var11d
           if var11d <> 4
               display "test77 no good " var11d
               add 1 to failure-count
           end-if
           move var24 to var12d
           if var12d <> 34
               display "test78 no good " var12d
               add 1 to failure-count
           end-if
           move var24 to var13d
           if var13d <> 234
               display "test79 no good " var13d
               add 1 to failure-count
           end-if
           move var24 to var14d
           if var14d <> 1234
               display "test80 no good " var14d
               add 1 to failure-count
           end-if
           move var21 to var21d
           if var21d <> 4
               display "test81 no good " var21d
               add 1 to failure-count
           end-if
           move var21 to var22d
           if var22d <> 4
               display "test82 no good " var22d
               add 1 to failure-count
           end-if
           move var21 to var23d
           if var23d <> 4
               display "test83 no good " var23d
               add 1 to failure-count
           end-if
           move var21 to var24d
           if var24d <> 4
               display "test84 no good " var24d
               add 1 to failure-count
           end-if
           move var22 to var21d
           if var21d <> 4
               display "test85 no good " var21d
               add 1 to failure-count
           end-if
           move var22 to var22d
           if var22d <> 34
               display "test86 no good " var22d
               add 1 to failure-count
           end-if
           move var22 to var23d
           if var23d <> 34
               display "test87 no good " var23d
               add 1 to failure-count
           end-if
           move var22 to var24d
           if var24d <> 34
               display "test88 no good " var24d
               add 1 to failure-count
           end-if
           move var23 to var21d
           if var21d <> 4
               display "test89 no good " var21d
               add 1 to failure-count
           end-if
           move var23 to var22d
           if var22d <> 34
               display "test90 no good " var22d
               add 1 to failure-count
           end-if
           move var23 to var23d
           if var23d <> 234
               display "test91 no good " var23d
               add 1 to failure-count
           end-if
           move var23 to var24d
           if var24d <> 234
               display "test92 no good " var24d
               add 1 to failure-count
           end-if
           move var24 to var21d
           if var21d <> 4
               display "test93 no good " var21d
               add 1 to failure-count
           end-if
           move var24 to var22d
           if var22d <> 34
               display "test94 no good " var22d
               add 1 to failure-count
           end-if
           move var24 to var23d
           if var23d <> 234
               display "test95 no good " var23d
               add 1 to failure-count
           end-if
           move var24 to var24d
           if var24d <> 1234
               display "test96 no good " var24d
               add 1 to failure-count
           end-if
           move var21 to var21nd
           if var21nd <> 4
               display "test97 no good " var21nd
               add 1 to failure-count
           end-if
           move var21 to var22nd
           if var22nd <> 4
               display "test98 no good " var22nd
               add 1 to failure-count
           end-if
           move var21 to var23nd
           if var23nd <> 4
               display "test99 no good " var23nd
               add 1 to failure-count
           end-if
           move var21 to var24nd
           if var24nd <> 4
               display "test100 no good " var24nd
               add 1 to failure-count
           end-if
           move var22 to var21nd
           if var21nd <> 4
               display "test101 no good " var21nd
               add 1 to failure-count
           end-if
           move var22 to var22nd
           if var22nd <> 34
               display "test102 no good " var22nd
               add 1 to failure-count
           end-if
           move var22 to var23nd
           if var23nd <> 34
               display "test103 no good " var23nd
               add 1 to failure-count
           end-if
           move var22 to var24nd
           if var24nd <> 34
               display "test104 no good " var24nd
               add 1 to failure-count
           end-if
           move var23 to var21nd
           if var21nd <> 4
               display "test105 no good " var21nd
               add 1 to failure-count
           end-if
           move var23 to var22nd
           if var22nd <> 34
               display "test106 no good " var22nd
               add 1 to failure-count
           end-if
           move var23 to var23nd
           if var23nd <> 234
               display "test107 no good " var23nd
               add 1 to failure-count
           end-if
           move var23 to var24nd
           if var24nd <> 234
               display "test108 no good " var24nd
               add 1 to failure-count
           end-if
           move var24 to var21nd
           if var21nd <> 4
               display "test109 no good " var21nd
               add 1 to failure-count
           end-if
           move var24 to var22nd
           if var22nd <> 34
               display "test110 no good " var22nd
               add 1 to failure-count
           end-if
           move var24 to var23nd
           if var23nd <> 234
               display "test111 no good " var23nd
               add 1 to failure-count
           end-if
           move var24 to var24nd
           if var24nd <> 1234
               display "test112 no good " var24nd
               add 1 to failure-count
           end-if
           move var21 to var31d
           if var31d <> 4
               display "test113 no good " var31d
               add 1 to failure-count
           end-if
           move var21 to var32d
           if var32d <> 4
               display "test114 no good " var32d
               add 1 to failure-count
           end-if
           move var21 to var33d
           if var33d <> 4
               display "test115 no good " var33d
               add 1 to failure-count
           end-if
           move var21 to var34d
           if var34d <> 4
               display "test116 no good " var34d
               add 1 to failure-count
           end-if
           move var22 to var31d
           if var31d <> 4
               display "test117 no good " var31d
               add 1 to failure-count
           end-if
           move var22 to var32d
           if var32d <> 34
               display "test118 no good " var32d
               add 1 to failure-count
           end-if
           move var22 to var33d
           if var33d <> 34
               display "test119 no good " var33d
               add 1 to failure-count
           end-if
           move var22 to var34d
           if var34d <> 34
               display "test120 no good " var34d
               add 1 to failure-count
           end-if
           move var23 to var31d
           if var31d <> 4
               display "test121 no good " var31d
               add 1 to failure-count
           end-if
           move var23 to var32d
           if var32d <> 34
               display "test122 no good " var32d
               add 1 to failure-count
           end-if
           move var23 to var33d
           if var33d <> 234
               display "test123 no good " var33d
               add 1 to failure-count
           end-if
           move var23 to var34d
           if var34d <> 234
               display "test124 no good " var34d
               add 1 to failure-count
           end-if
           move var24 to var31d
           if var31d <> 4
               display "test125 no good " var31d
               add 1 to failure-count
           end-if
           move var24 to var32d
           if var32d <> 34
               display "test126 no good " var32d
               add 1 to failure-count
           end-if
           move var24 to var33d
           if var33d <> 234
               display "test127 no good " var33d
               add 1 to failure-count
           end-if
           move var24 to var34d
           if var34d <> 1234
               display "test128 no good " var34d
               add 1 to failure-count
           end-if
           move var21n to var11d
           if var11d <> 4
               display "test129 no good " var11d
               add 1 to failure-count
           end-if
           move var21n to var12d
           if var12d <> 4
               display "test130 no good " var12d
               add 1 to failure-count
           end-if
           move var21n to var13d
           if var13d <> 4
               display "test131 no good " var13d
               add 1 to failure-count
           end-if
           move var21n to var14d
           if var14d <> 4
               display "test132 no good " var14d
               add 1 to failure-count
           end-if
           move var22n to var11d
           if var11d <> 4
               display "test133 no good " var11d
               add 1 to failure-count
           end-if
           move var22n to var12d
           if var12d <> 34
               display "test134 no good " var12d
               add 1 to failure-count
           end-if
           move var22n to var13d
           if var13d <> 34
               display "test135 no good " var13d
               add 1 to failure-count
           end-if
           move var22n to var14d
           if var14d <> 34
               display "test136 no good " var14d
               add 1 to failure-count
           end-if
           move var23n to var11d
           if var11d <> 4
               display "test137 no good " var11d
               add 1 to failure-count
           end-if
           move var23n to var12d
           if var12d <> 34
               display "test138 no good " var12d
               add 1 to failure-count
           end-if
           move var23n to var13d
           if var13d <> 234
               display "test139 no good " var13d
               add 1 to failure-count
           end-if
           move var23n to var14d
           if var14d <> 234
               display "test140 no good " var14d
               add 1 to failure-count
           end-if
           move var24n to var11d
           if var11d <> 4
               display "test141 no good " var11d
               add 1 to failure-count
           end-if
           move var24n to var12d
           if var12d <> 34
               display "test142 no good " var12d
               add 1 to failure-count
           end-if
           move var24n to var13d
           if var13d <> 234
               display "test143 no good " var13d
               add 1 to failure-count
           end-if
           move var24n to var14d
           if var14d <> 1234
               display "test144 no good " var14d
               add 1 to failure-count
           end-if
           move var21n to var21d
           if var21d <> -4
               display "test145 no good " var21d
               add 1 to failure-count
           end-if
           move var21n to var22d
           if var22d <> -4
               display "test146 no good " var22d
               add 1 to failure-count
           end-if
           move var21n to var23d
           if var23d <> -4
               display "test147 no good " var23d
               add 1 to failure-count
           end-if
           move var21n to var24d
           if var24d <> -4
               display "test148 no good " var24d
               add 1 to failure-count
           end-if
           move var22n to var21d
           if var21d <> -4
               display "test149 no good " var21d
               add 1 to failure-count
           end-if
           move var22n to var22d
           if var22d <> -34
               display "test150 no good " var22d
               add 1 to failure-count
           end-if
           move var22n to var23d
           if var23d <> -34
               display "test151 no good " var23d
               add 1 to failure-count
           end-if
           move var22n to var24d
           if var24d <> -34
               display "test152 no good " var24d
               add 1 to failure-count
           end-if
           move var23n to var21d
           if var21d <> -4
               display "test153 no good " var21d
               add 1 to failure-count
           end-if
           move var23n to var22d
           if var22d <> -34
               display "test154 no good " var22d
               add 1 to failure-count
           end-if
           move var23n to var23d
           if var23d <> -234
               display "test155 no good " var23d
               add 1 to failure-count
           end-if
           move var23n to var24d
           if var24d <> -234
               display "test156 no good " var24d
               add 1 to failure-count
           end-if
           move var24n to var21d
           if var21d <> -4
               display "test157 no good " var21d
               add 1 to failure-count
           end-if
           move var24n to var22d
           if var22d <> -34
               display "test158 no good " var22d
               add 1 to failure-count
           end-if
           move var24n to var23d
           if var23d <> -234
               display "test159 no good " var23d
               add 1 to failure-count
           end-if
           move var24n to var24d
           if var24d <> -1234
               display "test160 no good " var24d
               add 1 to failure-count
           end-if
           move var21n to var21nd
           if var21nd <> -4
               display "test161 no good " var21nd
               add 1 to failure-count
           end-if
           move var21n to var22nd
           if var22nd <> -4
               display "test162 no good " var22nd
               add 1 to failure-count
           end-if
           move var21n to var23nd
           if var23nd <> -4
               display "test163 no good " var23nd
               add 1 to failure-count
           end-if
           move var21n to var24nd
           if var24nd <> -4
               display "test164 no good " var24nd
               add 1 to failure-count
           end-if
           move var22n to var21nd
           if var21nd <> -4
               display "test165 no good " var21nd
               add 1 to failure-count
           end-if
           move var22n to var22nd
           if var22nd <> -34
               display "test166 no good " var22nd
               add 1 to failure-count
           end-if
           move var22n to var23nd
           if var23nd <> -34
               display "test167 no good " var23nd
               add 1 to failure-count
           end-if
           move var22n to var24nd
           if var24nd <> -34
               display "test168 no good " var24nd
               add 1 to failure-count
           end-if
           move var23n to var21nd
           if var21nd <> -4
               display "test169 no good " var21nd
               add 1 to failure-count
           end-if
           move var23n to var22nd
           if var22nd <> -34
               display "test170 no good " var22nd
               add 1 to failure-count
           end-if
           move var23n to var23nd
           if var23nd <> -234
               display "test171 no good " var23nd
               add 1 to failure-count
           end-if
           move var23n to var24nd
           if var24nd <> -234
               display "test172 no good " var24nd
               add 1 to failure-count
           end-if
           move var24n to var21nd
           if var21nd <> -4
               display "test173 no good " var21nd
               add 1 to failure-count
           end-if
           move var24n to var22nd
           if var22nd <> -34
               display "test174 no good " var22nd
               add 1 to failure-count
           end-if
           move var24n to var23nd
           if var23nd <> -234
               display "test175 no good " var23nd
               add 1 to failure-count
           end-if
           move var24n to var24nd
           if var24nd <> -1234
               display "test176 no good " var24nd
               add 1 to failure-count
           end-if
           move var21n to var31d
           if var31d <> 4
               display "test177 no good " var31d
               add 1 to failure-count
           end-if
           move var21n to var32d
           if var32d <> 4
               display "test178 no good " var32d
               add 1 to failure-count
           end-if
           move var21n to var33d
           if var33d <> 4
               display "test179 no good " var33d
               add 1 to failure-count
           end-if
           move var21n to var34d
           if var34d <> 4
               display "test180 no good " var34d
               add 1 to failure-count
           end-if
           move var22n to var31d
           if var31d <> 4
               display "test181 no good " var31d
               add 1 to failure-count
           end-if
           move var22n to var32d
           if var32d <> 34
               display "test182 no good " var32d
               add 1 to failure-count
           end-if
           move var22n to var33d
           if var33d <> 34
               display "test183 no good " var33d
               add 1 to failure-count
           end-if
           move var22n to var34d
           if var34d <> 34
               display "test184 no good " var34d
               add 1 to failure-count
           end-if
           move var23n to var31d
           if var31d <> 4
               display "test185 no good " var31d
               add 1 to failure-count
           end-if
           move var23n to var32d
           if var32d <> 34
               display "test186 no good " var32d
               add 1 to failure-count
           end-if
           move var23n to var33d
           if var33d <> 234
               display "test187 no good " var33d
               add 1 to failure-count
           end-if
           move var23n to var34d
           if var34d <> 234
               display "test188 no good " var34d
               add 1 to failure-count
           end-if
           move var24n to var31d
           if var31d <> 4
               display "test189 no good " var31d
               add 1 to failure-count
           end-if
           move var24n to var32d
           if var32d <> 34
               display "test190 no good " var32d
               add 1 to failure-count
           end-if
           move var24n to var33d
           if var33d <> 234
               display "test191 no good " var33d
               add 1 to failure-count
           end-if
           move var24n to var34d
           if var34d <> 1234
               display "test192 no good " var34d
               add 1 to failure-count
           end-if
           move var31 to var11d
           if var11d <> 4
               display "test193 no good " var11d
               add 1 to failure-count
           end-if
           move var31 to var12d
           if var12d <> 4
               display "test194 no good " var12d
               add 1 to failure-count
           end-if
           move var31 to var13d
           if var13d <> 4
               display "test195 no good " var13d
               add 1 to failure-count
           end-if
           move var31 to var14d
           if var14d <> 4
               display "test196 no good " var14d
               add 1 to failure-count
           end-if
           move var32 to var11d
           if var11d <> 4
               display "test197 no good " var11d
               add 1 to failure-count
           end-if
           move var32 to var12d
           if var12d <> 34
               display "test198 no good " var12d
               add 1 to failure-count
           end-if
           move var32 to var13d
           if var13d <> 34
               display "test199 no good " var13d
               add 1 to failure-count
           end-if
           move var32 to var14d
           if var14d <> 34
               display "test200 no good " var14d
               add 1 to failure-count
           end-if
           move var33 to var11d
           if var11d <> 4
               display "test201 no good " var11d
               add 1 to failure-count
           end-if
           move var33 to var12d
           if var12d <> 34
               display "test202 no good " var12d
               add 1 to failure-count
           end-if
           move var33 to var13d
           if var13d <> 234
               display "test203 no good " var13d
               add 1 to failure-count
           end-if
           move var33 to var14d
           if var14d <> 234
               display "test204 no good " var14d
               add 1 to failure-count
           end-if
           move var34 to var11d
           if var11d <> 4
               display "test205 no good " var11d
               add 1 to failure-count
           end-if
           move var34 to var12d
           if var12d <> 34
               display "test206 no good " var12d
               add 1 to failure-count
           end-if
           move var34 to var13d
           if var13d <> 234
               display "test207 no good " var13d
               add 1 to failure-count
           end-if
           move var34 to var14d
           if var14d <> 1234
               display "test208 no good " var14d
               add 1 to failure-count
           end-if
           move var31 to var21d
           if var21d <> 4
               display "test209 no good " var21d
               add 1 to failure-count
           end-if
           move var31 to var22d
           if var22d <> 4
               display "test210 no good " var22d
               add 1 to failure-count
           end-if
           move var31 to var23d
           if var23d <> 4
               display "test211 no good " var23d
               add 1 to failure-count
           end-if
           move var31 to var24d
           if var24d <> 4
               display "test212 no good " var24d
               add 1 to failure-count
           end-if
           move var32 to var21d
           if var21d <> 4
               display "test213 no good " var21d
               add 1 to failure-count
           end-if
           move var32 to var22d
           if var22d <> 34
               display "test214 no good " var22d
               add 1 to failure-count
           end-if
           move var32 to var23d
           if var23d <> 34
               display "test215 no good " var23d
               add 1 to failure-count
           end-if
           move var32 to var24d
           if var24d <> 34
               display "test216 no good " var24d
               add 1 to failure-count
           end-if
           move var33 to var21d
           if var21d <> 4
               display "test217 no good " var21d
               add 1 to failure-count
           end-if
           move var33 to var22d
           if var22d <> 34
               display "test218 no good " var22d
               add 1 to failure-count
           end-if
           move var33 to var23d
           if var23d <> 234
               display "test219 no good " var23d
               add 1 to failure-count
           end-if
           move var33 to var24d
           if var24d <> 234
               display "test220 no good " var24d
               add 1 to failure-count
           end-if
           move var34 to var21d
           if var21d <> 4
               display "test221 no good " var21d
               add 1 to failure-count
           end-if
           move var34 to var22d
           if var22d <> 34
               display "test222 no good " var22d
               add 1 to failure-count
           end-if
           move var34 to var23d
           if var23d <> 234
               display "test223 no good " var23d
               add 1 to failure-count
           end-if
           move var34 to var24d
           if var24d <> 1234
               display "test224 no good " var24d
               add 1 to failure-count
           end-if
           move var31 to var21nd
           if var21nd <> 4
               display "test225 no good " var21nd
               add 1 to failure-count
           end-if
           move var31 to var22nd
           if var22nd <> 4
               display "test226 no good " var22nd
               add 1 to failure-count
           end-if
           move var31 to var23nd
           if var23nd <> 4
               display "test227 no good " var23nd
               add 1 to failure-count
           end-if
           move var31 to var24nd
           if var24nd <> 4
               display "test228 no good " var24nd
               add 1 to failure-count
           end-if
           move var32 to var21nd
           if var21nd <> 4
               display "test229 no good " var21nd
               add 1 to failure-count
           end-if
           move var32 to var22nd
           if var22nd <> 34
               display "test230 no good " var22nd
               add 1 to failure-count
           end-if
           move var32 to var23nd
           if var23nd <> 34
               display "test231 no good " var23nd
               add 1 to failure-count
           end-if
           move var32 to var24nd
           if var24nd <> 34
               display "test232 no good " var24nd
               add 1 to failure-count
           end-if
           move var33 to var21nd
           if var21nd <> 4
               display "test233 no good " var21nd
               add 1 to failure-count
           end-if
           move var33 to var22nd
           if var22nd <> 34
               display "test234 no good " var22nd
               add 1 to failure-count
           end-if
           move var33 to var23nd
           if var23nd <> 234
               display "test235 no good " var23nd
               add 1 to failure-count
           end-if
           move var33 to var24nd
           if var24nd <> 234
               display "test236 no good " var24nd
               add 1 to failure-count
           end-if
           move var34 to var21nd
           if var21nd <> 4
               display "test237 no good " var21nd
               add 1 to failure-count
           end-if
           move var34 to var22nd
           if var22nd <> 34
               display "test238 no good " var22nd
               add 1 to failure-count
           end-if
           move var34 to var23nd
           if var23nd <> 234
               display "test239 no good " var23nd
               add 1 to failure-count
           end-if
           move var34 to var24nd
           if var24nd <> 1234
               display "test240 no good " var24nd
               add 1 to failure-count
           end-if
           move var31 to var31d
           if var31d <> 4
               display "test241 no good " var31d
               add 1 to failure-count
           end-if
           move var31 to var32d
           if var32d <> 4
               display "test242 no good " var32d
               add 1 to failure-count
           end-if
           move var31 to var33d
           if var33d <> 4
               display "test243 no good " var33d
               add 1 to failure-count
           end-if
           move var31 to var34d
           if var34d <> 4
               display "test244 no good " var34d
               add 1 to failure-count
           end-if
           move var32 to var31d
           if var31d <> 4
               display "test245 no good " var31d
               add 1 to failure-count
           end-if
           move var32 to var32d
           if var32d <> 34
               display "test246 no good " var32d
               add 1 to failure-count
           end-if
           move var32 to var33d
           if var33d <> 34
               display "test247 no good " var33d
               add 1 to failure-count
           end-if
           move var32 to var34d
           if var34d <> 34
               display "test248 no good " var34d
               add 1 to failure-count
           end-if
           move var33 to var31d
           if var31d <> 4
               display "test249 no good " var31d
               add 1 to failure-count
           end-if
           move var33 to var32d
           if var32d <> 34
               display "test250 no good " var32d
               add 1 to failure-count
           end-if
           move var33 to var33d
           if var33d <> 234
               display "test251 no good " var33d
               add 1 to failure-count
           end-if
           move var33 to var34d
           if var34d <> 234
               display "test252 no good " var34d
               add 1 to failure-count
           end-if
           move var34 to var31d
           if var31d <> 4
               display "test253 no good " var31d
               add 1 to failure-count
           end-if
           move var34 to var32d
           if var32d <> 34
               display "test254 no good " var32d
               add 1 to failure-count
           end-if
           move var34 to var33d
           if var33d <> 234
               display "test255 no good " var33d
               add 1 to failure-count
           end-if
           move var34 to var34d
           if var34d <> 1234
               display "test256 no good " var34d
               add 1 to failure-count
           end-if
           if failure-count = 0
               display "All 256 packed-decimal MOVE tests passed"
           else
               display failure-count
                   " packed-decimal MOVE tests failed"
           end-if
           move failure-count to return-code
           goback.

