[
  Emit the uppercase alphabet
]

cell 0 = 26
++++++++++++++++++++++++++

cell 1 = 65
>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++<

while cell#0 != 0
[
 >
 .      emit cell#1
 +      increment cell@1
 <-     decrement cell@0
]
