// { dg-additional-options "-fmodules-ts" }

export module bob;
// { dg-module-cmi bob }

export struct import {};
export ::import *a;
export ::import (b);
