typedef struct _GdkDrawable GdkDrawable; 
typedef struct _GdkDrawable GdkBitmap; 
typedef struct _GdkDrawable GdkPixmap; 
 
class Drawable 
{ 
public: 
 operator GdkDrawable* () const; 
}; 
 
 
class Pixmap : public Drawable 
{ 
public: 
 operator GdkPixmap* () const; 
 
}; 
 
 
class Bitmap : public Pixmap 
{ 
public: 
  operator GdkBitmap* () const; 
 
}; 
 
class Event 
{ 
}; 
 
Bitmap::operator GdkBitmap* () const 
{ 
 return  0; 
} 
