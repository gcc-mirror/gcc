// { dg-additional-options -fmodules-atom }
// we ICED on malformed preambles ending at EOF.
import bob // { dg-error "expected" }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error:" }
// { dg-error "failed to read" "" { target *-*-* } 0 }
