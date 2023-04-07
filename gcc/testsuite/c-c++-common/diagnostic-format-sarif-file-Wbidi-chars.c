/* Adapted from Wbidi-chars-1.c */

/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

int main() {
    int isAdmin = 0;
    /*‮ } ⁦if (isAdmin)⁩ ⁦ begin admins only */
        __builtin_printf("You are an admin.\n");
    /* end admins only ‮ { ⁦*/
    return 0;
}

/* Verify that we generate a valid UTF-8 .sarif file.

     { dg-final { verify-sarif-file } }

   Verify that we captured the expected warnings.

     { dg-final { scan-sarif-file {"text": "unpaired UTF-8 bidirectional control characters detected"} } }
     { dg-final { scan-sarif-file {"text": "unpaired UTF-8 bidirectional control characters detected"} } }
*/
