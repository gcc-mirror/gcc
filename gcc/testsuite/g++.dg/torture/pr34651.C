/* { dg-do compile } */

typedef bool Bool;
struct CString {
    CString (const char * =__null);
    CString & operator += (const CString &);
};
struct THotKey {
   short Key;
   Bool Control;
   Bool Shift;
   Bool Alt;
};
THotKey m_HotKey;
THotKey GetHotKey () { return m_HotKey; }
void Serialize ()
{
   THotKey inHotKey (GetHotKey());
   CString outCombinaison (inHotKey.Control
			   ? ((inHotKey.Alt || inHotKey.Shift)
			      ? "ctrl+" : "ctrl")
			   : __null);
   outCombinaison += inHotKey.Alt ? inHotKey.Shift ? "alt+" : "alt" : "";
   outCombinaison += inHotKey.Shift ? "shift" : "";
}
