// { dg-prune-output "compilation terminated" }
// Depending on timing, we either get a crc mismatch or notice the timestamp
import Bar; // { dg-error "CRC mismatch" }
// { dg-error "bad file data|failed to read" "" { target *-*-* } .-1 }

int bill ();
