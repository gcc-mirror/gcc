       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  FIELD         PIC S9(4)V9(2) COMP-5.
       01  FIELD-DISP    PIC S9(4)V9(2) DISPLAY.
       PROCEDURE        DIVISION.
           MOVE 0.2 TO FIELD.
           ADD 1
               2
               3
               4
               5
               6
               7
               8
               9
               10
               11
               12
               13
               14
               15
               16
               17
               18
               19
               20
               21
               22
               23
               24
               25
               26
               27
               28
               29
               30
               31
               32
               33
               34
               35
               36
               37
               38
               39
               40
               41
               42
               43
               44
               45
               46
               47
               48
               49
               50
               51
               52
               53
               54
               55
               56
               57
               58
               59
               60
               61
               62
               63
               64
               65
               66
               67
               68
               69
               70
               71
               72
               73
               74
               75
               76
               77
               78
               79
               80
               81
               82
               83
               84
               85
               86
               87
               88
               89
               90
               91
               92
               93
               94
               95
               96
               97
               98
               99
               100
               101
               102
               103
               104
               105
               106
               107
               108
               109
               110
               111
               112
               113
               114
               115
               116
               117
               118
               119
               120
               121
               122
               123
               124
               125
               126
               127
               128
               129
               TO FIELD
           END-ADD.
           IF FIELD NOT = 8385.2
              MOVE FIELD TO FIELD-DISP
              DISPLAY 'ADD with wrong result: ' FIELD-DISP
              END-DISPLAY
           END-IF.
           COMPUTE FIELD = (0.2
                         + 2
                         + 3
                         + 4
                         + 5
                         + 6
                         + 7
                         + 8
                         + 9
                         + 10
                         + 11
                         + 12
                         + 13
                         + 14
                         + 15
                         + 16
                         + 17
                         + 18
                         + 19
                         + 20
                         + 21
                         + 22
                         + 23
                         + 24
                         + 25
                         + 26
                         + 27
                         + 28
                         + 29
                         + 30
                         + 31
                         + 32
                         + 33
                         + 34
                         + 35
                         + 36
                         + 37
                         + 38
                         + 39
                         + 40
                         + 41
                         + 42
                         + 43
                         + 44
                         + 45
                         + 46
                         + 47
                         + 48
                         + 49
                         + 50
                         + 51
                         + 52
                         + 53
                         + 54
                         + 55
                         + 56
                         + 57
                         + 58
                         - 59
                         - 60
                         - 61
                         - 62
                         - 63
                         - 64
                         - 65
                         - 66
                         - 67
                         - 68
                         - 69
                         - 70
                         - 71
                         - 72
                         - 73
                         - 74
                         - 75
                         - 76
                         - 77
                         - 78
                         - 79
                         - 80
                         - 81
                         - 82
                         - 83
                         - 84
                         - 85
                         - 86
                         - 87
                         - 88
                         - 89
                         - 90
                         - 91
                         - 92
                         - 93
                         - 94
                         - 95
                         - 96
                         - 97
                         - 98
                         - 99
                         - 100
                         - 101
                         - 102
                         - 103
                         - 104
                         - 105
                         - 106
                         - 107
                         - 108
                         - 109
                         - 110
                         - 111
                         - 112
                         - 113
                         - 114
                         - 115
                         - 116
                         - 117
                         - 118
                         - 119
                         - 120
                         - 121
                         - 122
                         - 123
                         - 124
                         - 125
                         - 126
                         - 127)
                         * 12800000000
                         / 12900000000
           END-COMPUTE.
           IF FIELD NOT = -4670.31
              MOVE FIELD TO FIELD-DISP
              DISPLAY 'COMPUTE with wrong result: ' FIELD-DISP
              END-DISPLAY
           END-IF.
           GOBACK.

