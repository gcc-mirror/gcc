// { dg-additional-options -fmodules-ts }
export  module  hello;
// { dg-module-cmi hello }
export import :check;
import  format;

export namespace NS
{
using NS::Format;
}
