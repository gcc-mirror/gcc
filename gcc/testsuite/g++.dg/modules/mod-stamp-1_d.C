// { dg-prune-output "compilation terminated" }
// Depending on timing, we either get a crc mismatch or notice the timestamp
import Bar;
// { dg-error "CRC mismatch" "Foo.nms:" { target *-*-* } 0 }
// { dg-error "bad file data|failed to read" "Foo.nms:" { target *-*-* } 0 }

int bill ();
