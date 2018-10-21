// { dg-additional-options "-fmodules-ts -fno-module-keywords" }

export module bob;
// { dg-module-bmi bob }

export struct import {};
export import *a;
export import (b);
