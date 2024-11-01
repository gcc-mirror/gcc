// { dg-additional-options "-fmodules-ts" }

#define bob() fred
export module bob;

// { dg-module-cmi bob }
