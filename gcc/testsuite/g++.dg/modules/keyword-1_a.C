// { dg-additional-options "-fmodules-ts" }

export module bob;
// { dg-module-bmi bob }

export struct import {};
export ::import *a;
export ::import (b);
