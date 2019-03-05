/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-mcpu=thunderx+cripto" } */

void
foo ()
{
}

/* { dg-error "invalid feature modifier .cripto. in .-mcpu=thunderx\\+cripto."  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments are: \[^\n\r]*; did you mean .crypto.?"  "" { target *-*-* } 0 } */

