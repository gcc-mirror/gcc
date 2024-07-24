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

   Verify that the expected property bag property is present.
     { dg-final { scan-sarif-file {"gcc/escapeNonAscii": true} } }

   Verify that the snippets have a "rendered" property.
   We check the contents of the property via a selftest.

     { dg-final { scan-sarif-file {"rendered": } } }

   Verify that we have an "annotations" property for the
   labelled ranges (3.28.6).
     { dg-final { scan-sarif-file {"annotations": } } }
   and that the annotations capture the labels as messages,
   using "." in place of awkard characters:
     { dg-final { scan-sarif-file {"message": ."text": "end of bidirectional context"} } }
     { dg-final { scan-sarif-file {"message": ."text": "U.202E .RIGHT-TO-LEFT OVERRIDE."} } }
     { dg-final { scan-sarif-file {"message": ."text": "U.2066 .LEFT-TO-RIGHT ISOLATE."} } }
*/
