// https://bugzilla.gdcproject.org/show_bug.cgi?id=231
// { dg-additional-sources "imports/gdc231.d" }
// { dg-do compile }

import imports.gdc231;

class Range231 : Widget231
{
    override void* getStruct()
    {
        return null;
    }
}
