/* PR optimization/10467  */
/* { dg-do compile { target arm*-*-* xscale*-*-* strongarm*-*-* } } */
/* { dg-options "-O2 -mthumb" } */

typedef enum {Ident_1} Enumeration;

typedef struct record
{
    struct record *Ptr_Comp;
    Enumeration Discr;
    union {
	struct {
	    Enumeration Enum_Comp;
	    int Int_Comp;
	    char Str_Comp [31];
        } var_1;
    } variant;
} *Rec_Pointer;

Rec_Pointer Ptr_Glob;

Proc_1 (Ptr_Val_Par)
    Rec_Pointer Ptr_Val_Par;
{
    Rec_Pointer Next_Record = Ptr_Val_Par->Ptr_Comp;

    *Ptr_Val_Par->Ptr_Comp = *Ptr_Glob;

    if (Next_Record->Discr == Ident_1)
    {
	Proc_7 (Next_Record->variant.var_1.Int_Comp, 10,
		&Next_Record->variant.var_1.Int_Comp);
    }
}

