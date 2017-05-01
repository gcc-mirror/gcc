
// Depending on timing, we either get a crc mismatch or notice the timestamp
import Bar; // { dg-error "time stamp|crc mismatch" }
// { dg-error "failed to read" "" { target *-*-* } .-1 }

int bill ();
