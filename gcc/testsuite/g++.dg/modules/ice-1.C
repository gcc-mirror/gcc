// { dg-additional-options "-fmodules-ts" }
// we ICED on malformed preambles ending at EOF.
import bob // { dg-error "expected" }
