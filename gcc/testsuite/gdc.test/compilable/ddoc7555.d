// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 7555
module ddoc7555;

/**
Dummy doc.

$(X0
  DelimitedString
  TokenString)

$(X1
  DelimitedString
  TokenString)

$(X2 x,HexString)
$(X2 x, HexString)

$(X3 x,x,HexString)
$(X3 x,x, HexString)

$(X4 x,x,x,HexString)
$(X4 x,x,x, HexString)

$(X5 x,x,x,x,HexString)
$(X5 x,x,x,x, HexString)

$(X6 x,x,x,x,x,HexString)
$(X6 x,x,x,x,x, HexString)

$(X7 x,x,x,x,x,x,HexString)
$(X7 x,x,x,x,x,x, HexString)

$(X8 x,x,x,x,x,x,x,HexString)
$(X8 x,x,x,x,x,x,x, HexString)

$(X9 x,x,x,x,x,x,x,x,HexString)
$(X9 x,x,x,x,x,x,x,x, HexString)

Macros:
    X0=$0
    X1=$1
    X2=$2
    X3=$3
    X4=$4
    X5=$5
    X6=$6
    X7=$7
    X8=$8
    X9=$9
*/
void dummy();
