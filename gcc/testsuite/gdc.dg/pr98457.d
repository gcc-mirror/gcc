// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98457
// { dg-do compile }

void main()
{
    writef!"%s";    // { dg-error "template instance writef!\"%s\" template .writef. is not defined" }
    writef!"`%s";   // { dg-error "template instance writef!\"`%s\" template .writef. is not defined" }
    writef!"%%s`";  // { dg-error "template instance writef!\"%%s`\" template .writef. is not defined" }
}
